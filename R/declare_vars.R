# ---------------------------------------------------------------------------
# DECLARE variable extraction and substitution
#
# Two responsibilities:
#   (a) extract_declare(): parse DECLARE @var TYPE [= value_expr] and
#       SET @var = value_expr statements into a variable registry.
#   (b) substitute_vars(): replace downstream @var references with either the
#       literal value (safe to pass verbatim) or a typed sentinel literal
#       (for complex expressions like DATEADD(...) that would crash sqlglot).
#
# Removing bare @variable references before a SELECT reaches the Python
# tokeniser is the single biggest crash-avoidance win in the whole pipeline.
# ---------------------------------------------------------------------------

# Typed sentinel literals used when a DECLARE value is not a simple literal.
# Chosen to be valid SQL tokens of the declared type.
.SENTINELS <- c(
  DATE     = "'1900-01-01'",
  DATETIME = "'1900-01-01 00:00:00'",
  DATETIME2 = "'1900-01-01 00:00:00'",
  SMALLDATETIME = "'1900-01-01 00:00:00'",
  TIME     = "'00:00:00'",
  INT      = "0",
  BIGINT   = "0",
  SMALLINT = "0",
  TINYINT  = "0",
  BIT      = "0",
  FLOAT    = "0.0",
  REAL     = "0.0",
  DECIMAL  = "0",
  NUMERIC  = "0",
  MONEY    = "0",
  SMALLMONEY = "0",
  VARCHAR  = "''",
  NVARCHAR = "N''",
  CHAR     = "''",
  NCHAR    = "N''",
  TEXT     = "''",
  NTEXT    = "N''",
  UNIQUEIDENTIFIER = "'00000000-0000-0000-0000-000000000000'"
)

# Return the sentinel literal for a declared SQL type, falling back to NULL
# (which callers treat as "leave the @var unreplaced").
sentinel_for_type <- function(type_str) {
  key <- toupper(trimws(type_str))
  as.list(.SENTINELS)[[key]]  # returns NULL for unknown types; safe [[
}

# Determine if a value expression is a "simple literal": a string/date literal
# (optionally wrapped in a single CAST), or a numeric literal. These are safe
# to substitute verbatim. Complex expressions (DATEADD, GETDATE, arithmetic)
# use the type sentinel instead.
is_literal <- function(expr) {
  if (is.null(expr) || is.na(expr) || !nzchar(trimws(expr))) return(FALSE)
  e <- trimws(expr)
  # Bare numeric literal (integer or decimal, optional sign).
  if (grepl("^[+-]?[0-9]+(\\.[0-9]+)?$", e))   return(TRUE)
  # Bare string / date literal N'...' or '...'.
  if (grepl("^N?'.*'$", e, perl = TRUE))         return(TRUE)
  # CAST(literal AS type) — one level of wrapping is fine.
  inner <- sub("^CAST\\((.+) AS [A-Za-z0-9()]+\\)$", "\\1", e,
               ignore.case = TRUE, perl = TRUE)
  if (!identical(inner, e)) return(is_literal(inner))
  FALSE
}

#' Extract variable declarations from a single SQL statement
#'
#' Parses a `DECLARE @var TYPE [= value_expr]` or `SET @var = value_expr`
#' statement and returns a one-row tibble describing the variable. Returns a
#' zero-row tibble for statements that are not DECLARE/SET.
#'
#' @param text A length-1 character vector containing one SQL statement.
#'
#' @return A tibble with columns `name` (chr, e.g. `"@StartDate"`), `type`
#'   (chr, e.g. `"DATE"`), `value_expr` (chr, the raw value expression or
#'   `NA`), `is_literal` (lgl).
#' @export
extract_declare <- function(text) {
  empty <- tibble::tibble(
    name       = character(),
    type       = character(),
    value_expr = character(),
    is_literal = logical()
  )

  t <- trimws(text)

  # --- DECLARE @var TYPE [= expr] ---
  # Pattern: DECLARE @name TYPE_TOKEN [= rest]
  # We allow optional (precision) after the type, e.g. VARCHAR(50).
  m <- regexpr(
    paste0(
      "(?i)^DECLARE\\s+(@[A-Za-z0-9_]+)\\s+",
      "([A-Za-z]+(?:\\([^)]*\\))?)\\s*",     # type (with optional parens)
      "(?:=\\s*(.+))?$"                       # optional = value_expr
    ),
    t, perl = TRUE
  )
  if (m > 0L) {
    caps <- regmatches(t, m)
    # Re-extract capture groups individually for robustness.
    nm <- sub(
      paste0("(?i)^DECLARE\\s+(@[A-Za-z0-9_]+).*"), "\\1", t, perl = TRUE
    )
    ty <- sub(
      paste0("(?i)^DECLARE\\s+@[A-Za-z0-9_]+\\s+",
             "([A-Za-z]+(?:\\([^)]*\\))?).*"),
      "\\1", t, perl = TRUE
    )
    # Value expr: everything after the first =, if present.
    ve_raw <- sub(
      paste0("(?i)^DECLARE\\s+@[A-Za-z0-9_]+\\s+",
             "[A-Za-z]+(?:\\([^)]*\\))?\\s*(?:=\\s*(.+))?$"),
      "\\1", t, perl = TRUE
    )
    ve <- if (identical(ve_raw, t) || !nzchar(trimws(ve_raw))) {
      NA_character_
    } else {
      trimws(ve_raw)
    }
    return(tibble::tibble(
      name       = nm,
      type       = toupper(sub("\\(.*", "", ty)),  # strip (precision)
      value_expr = ve,
      is_literal = is_literal(ve)
    ))
  }

  # --- SET @var = expr ---
  m2 <- regexpr(
    "(?i)^SET\\s+(@[A-Za-z0-9_]+)\\s*=\\s*(.+)$",
    t, perl = TRUE
  )
  if (m2 > 0L) {
    nm <- sub("(?i)^SET\\s+(@[A-Za-z0-9_]+).*", "\\1", t, perl = TRUE)
    ve <- trimws(sub(
      "(?i)^SET\\s+@[A-Za-z0-9_]+\\s*=\\s*(.+)$", "\\1", t, perl = TRUE
    ))
    return(tibble::tibble(
      name       = nm,
      type       = NA_character_,   # SET doesn't declare the type
      value_expr = ve,
      is_literal = is_literal(ve)
    ))
  }

  empty
}

#' Substitute @variable references in a SQL string
#'
#' Replaces each `@variable` reference with its registered value, following
#' the policy:
#' - **Literal** values (plain string/number/date, optionally one CAST) →
#'   substituted verbatim, preserving the exact filter predicate.
#' - **Non-literal** values (DATEADD, GETDATE, arithmetic, ...) → replaced
#'   by a typed sentinel chosen by the variable's declared type. This removes
#'   crash-inducing T-SQL expressions before they reach the Python tokeniser
#'   while keeping the predicate shape intact.
#' - Variables with unknown types and non-literal values → left unsubstituted
#'   (callers should log these as "could not resolve").
#'
#' Matching is word-boundary, `@`-anchored: `@StartDate` will NOT match a
#' longer `@StartDateTime`.
#'
#' @param text A length-1 character vector.
#' @param variables A tibble as returned by [extract_declare()] (multiple rows
#'   accepted; processed in the order given).
#'
#' @return The text with `@variable` references replaced.
#' @export
substitute_vars <- function(text, variables) {
  if (nrow(variables) == 0L || !nzchar(text)) return(text)

  for (i in seq_len(nrow(variables))) {
    var  <- variables$name[i]
    ve   <- variables$value_expr[i]
    type <- variables$type[i]
    lit  <- variables$is_literal[i]

    replacement <- if (!is.na(ve) && lit) {
      ve  # safe to use verbatim
    } else if (!is.na(type)) {
      sentinel_for_type(type)  # NULL if type unknown
    } else {
      NULL
    }

    if (!is.null(replacement)) {
      # Word-boundary, @-anchored replacement.
      # (?<![A-Za-z0-9_@]) ensures we don't match a substring like
      # @StartDateTime when replacing @StartDate.
      pat <- paste0("(?<![A-Za-z0-9_@])", stringr::fixed(var),
                    "(?![A-Za-z0-9_])")
      # stringr::fixed() is for the var name, but we need regex for the
      # anchors — use str_replace_all with a regex that quotes the var name.
      escaped <- paste0("(?<![A-Za-z0-9_@])",
                        stringr::str_escape(var),
                        "(?![A-Za-z0-9_])")
      text <- stringr::str_replace_all(text, escaped, replacement)
    }
  }
  text
}

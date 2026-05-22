# ---------------------------------------------------------------------------
# Parser adapter
#
# Wraps the Python `sqlglot` library (via reticulate) to parse and qualify a
# SQL script. All sqlglot-specific AST traversal lives in the bundled Python
# module `inst/python/rdataflow_sqlglot.py`; this file is only responsible for
# locating Python, converting the schema object into the nested mapping
# sqlglot expects, and returning the extracted structure to R.
# ---------------------------------------------------------------------------

# Package-level cache for the imported Python module so we import it once per
# session rather than on every parse_sql() call.
.rdataflow_py <- new.env(parent = emptyenv())

#' Is the Python `sqlglot` library available?
#'
#' @return `TRUE` if reticulate can see a `sqlglot` module, else `FALSE`.
#' @export
sqlglot_available <- function() {
  requireNamespace("reticulate", quietly = TRUE) &&
    reticulate::py_module_available("sqlglot")
}

#' Install the Python `sqlglot` library
#'
#' Convenience wrapper around [reticulate::py_install()] for the one Python
#' dependency the package needs. By default it installs into reticulate's
#' managed environment.
#'
#' @param method,conda Passed through to [reticulate::py_install()].
#' @param ... Further arguments to [reticulate::py_install()].
#'
#' @return Invisibly `NULL`; called for its side effect.
#' @export
install_sqlglot <- function(method = "auto", conda = "auto", ...) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    rlang::abort("Package 'reticulate' is required to install sqlglot.")
  }
  reticulate::py_install("sqlglot", method = method, conda = conda, ...)
  invisible(NULL)
}

# Locate and import the bundled Python extractor module. We look first in the
# installed package (inst/python -> python), then fall back to the source
# tree so the function works under devtools::load_all() / direct sourcing.
load_py_module <- function() {
  if (!is.null(.rdataflow_py$mod)) {
    return(.rdataflow_py$mod)
  }
  if (!sqlglot_available()) {
    rlang::abort(paste(
      "Python 'sqlglot' is not available.",
      "Install it with Rdataflow::install_sqlglot()."
    ))
  }

  path <- find_py_path()
  mod <- reticulate::import_from_path("rdataflow_sqlglot", path = path)
  .rdataflow_py$mod <- mod
  mod
}

# Locate the directory containing the bundled Python module. Prefer the
# installed package location; otherwise walk up from the working directory
# looking for inst/python (so the module is found under devtools::load_all(),
# direct sourcing, or testthat, which runs from tests/testthat/).
find_py_path <- function() {
  module_file <- "rdataflow_sqlglot.py"

  installed <- system.file("python", package = "Rdataflow")
  if (nzchar(installed) && file.exists(file.path(installed, module_file))) {
    return(installed)
  }

  # Walk up a bounded number of parent directories from the working dir.
  dir <- normalizePath(getwd(), mustWork = FALSE)
  for (i in seq_len(6L)) {
    candidate <- file.path(dir, "inst", "python")
    if (file.exists(file.path(candidate, module_file))) {
      return(candidate)
    }
    parent <- dirname(dir)
    if (identical(parent, dir)) break  # reached filesystem root
    dir <- parent
  }

  rlang::abort("Could not locate the bundled 'rdataflow_sqlglot.py' module.")
}

#' Parse a SQL script into per-statement lineage information
#'
#' Parses (and, when a schema is supplied, qualifies) a SQL script using
#' `sqlglot`, returning a nested list describing each statement's stages,
#' projections, sources, joins, group-bys, and filters. This is the raw
#' structure consumed by [build_ir()].
#'
#' Procedural T-SQL constructs (`DECLARE`, `SET`, `CREATE TABLE`, `DROP TABLE`,
#' `INSERT … VALUES`, `CREATE INDEX`) are handled natively in R before any SQL
#' reaches Python. Each SELECT-bearing statement is sent to the Python/sqlglot
#' parser inside a [callr::r_session] subprocess so that a C-level crash in
#' sqlglot cannot kill the R session.
#'
#' @param sql A SQL script as a length-1 character vector.
#' @param schema An optional `rdataflow_schema` object (see
#'   [schema_from_list()] / [schema_from_con()]). When supplied, `*` is
#'   expanded and unqualified columns are resolved to their tables.
#' @param dialect sqlglot dialect name; defaults to `"tsql"`.
#'
#' @return A nested list with a `statements` element. Each statement carries
#'   `index`, `kind`, `output_table`, and a list of `stages`. A `skipped`
#'   element lists any statements that could not be parsed (non-fatal).
#' @export
parse_sql <- function(sql, schema = NULL, dialect = "tsql") {
  stopifnot(is.character(sql), length(sql) == 1)

  # --- Step 1: split + classify -------------------------------------------
  stmts    <- classify_statements(split_statements(sql))
  skipped  <- character()

  # Kinds that route to sqlglot (SELECT-bearing statements).
  select_kinds <- c("select", "select_into", "insert_select")

  # --- Step 2: thread registries forward through the script ---------------
  var_registry  <- tibble::tibble(
    name = character(), type = character(),
    value_expr = character(), is_literal = logical()
  )
  temp_registry <- new_temp_registry()

  statements_out <- vector("list", nrow(stmts))

  for (i in seq_len(nrow(stmts))) {
    row  <- stmts[i, ]
    kind <- row$kind
    text <- row$text
    seq  <- row$seq

    if (kind == "declare") {
      # --- native: accumulate variable registry ---
      new_var <- extract_declare(text)
      if (nrow(new_var) > 0L) {
        var_registry <- dplyr::bind_rows(var_registry, new_var)
      }
      next

    } else if (kind == "set_var") {
      # --- native: update variable registry with new value ---
      new_var <- extract_declare(text)
      if (nrow(new_var) > 0L) {
        # SET updates the value for an already-declared variable. If the name
        # exists in the registry, update it; otherwise append.
        existing_idx <- which(var_registry$name == new_var$name[1])
        if (length(existing_idx) > 0L) {
          var_registry$value_expr[existing_idx[1]] <- new_var$value_expr[1]
          var_registry$is_literal[existing_idx[1]] <- new_var$is_literal[1]
        } else {
          var_registry <- dplyr::bind_rows(var_registry, new_var)
        }
      }
      next

    } else if (kind == "create_table") {
      # --- native: parse column list and register in temp schema ---
      cols <- parse_create_table_columns(text)
      tbl  <- extract_output_table(text, kind)
      if (!is.null(tbl) && nchar(tbl) > 0L) {
        temp_registry <- register_temp_table(temp_registry, tbl, cols,
                                             origin_seq = seq)
      }
      next

    } else if (kind %in% c("drop", "create_index", "insert_values", "unknown")) {
      # --- skip: no lineage contribution ---
      skipped <- c(skipped,
                   sprintf("seq %d (%s): skipped non-SELECT statement", seq, kind))
      next

    } else if (kind %in% select_kinds) {
      # --- SELECT path: substitute vars, merge temp schema, then parse ---

      # Variable substitution: removes @vars and complex T-SQL expressions
      # before the text reaches the Python tokeniser.
      clean_text <- substitute_vars(text, var_registry)

      # Determine output_table (for INSERT INTO … SELECT or SELECT … INTO).
      output_tbl <- extract_output_table(clean_text, kind)

      # Merge temp schema so sqlglot can qualify temp-table references.
      merged_schema <- filter_schema_to_sql(clean_text, schema)
      merged_schema <- merge_temp_schema(clean_text, merged_schema, temp_registry)

      # Parse inside subprocess — crash-safe.
      iso <- parse_one_select_isolated(clean_text, schema = merged_schema,
                                       dialect = dialect,
                                       skipped_log = skipped)
      skipped <- iso$skipped_log
      parsed  <- iso$result

      if (is.null(parsed) || length(parsed$statements) == 0L) {
        skipped <- c(skipped,
                     sprintf("seq %d (%s): no lineage extracted", seq, kind))
        next
      }

      # sqlglot may return multiple statements for one input (rare); take all.
      for (st in parsed$statements) {
        # Override kind and output_table with our R-level classification,
        # which is more reliable for procedural scripts.
        st$kind         <- kind
        st$output_table <- output_tbl %||% st$output_table
        statements_out[[length(Filter(Negate(is.null), statements_out)) + 1L]] <- st
      }

      # After a SELECT INTO / INSERT SELECT, register the output columns in
      # the temp registry so downstream statements can resolve them.
      if (!is.null(output_tbl) && nzchar(output_tbl)) {
        temp_registry <- register_select_output(
          temp_registry, parsed, output_tbl, origin_seq = seq
        )
      }

    } else {
      skipped <- c(skipped,
                   sprintf("seq %d (unhandled kind '%s'): skipped", seq, kind))
    }
  }

  # Re-index the statements list (some entries may be NULL from skips above).
  out_stmts <- Filter(Negate(is.null), statements_out)
  for (j in seq_along(out_stmts)) {
    out_stmts[[j]]$index <- j
  }

  list(statements = out_stmts, skipped = skipped)
}

# Null-coalescing operator: return lhs unless it is NULL, then return rhs.
`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs

# Extract the output table name from a statement, given its classified kind.
# Returns NULL when the statement has no output table.
extract_output_table <- function(text, kind) {
  if (kind == "select_into") {
    # SELECT ... INTO #name ...
    m <- regexpr("(?i)\\bINTO\\s+([A-Za-z_#][A-Za-z0-9_#.]*)", text, perl = TRUE)
    if (m > 0L) {
      raw <- regmatches(text, m)
      return(trimws(sub("(?i)^INTO\\s+", "", raw, perl = TRUE)))
    }
    # CREATE TABLE #name AS SELECT ...
    m2 <- regexpr(
      "(?i)^CREATE\\s+TABLE\\s+([A-Za-z_#][A-Za-z0-9_#.]*)", text, perl = TRUE
    )
    if (m2 > 0L) {
      raw <- regmatches(text, m2)
      return(trimws(sub("(?i)^CREATE\\s+TABLE\\s+", "", raw, perl = TRUE)))
    }
    return(NULL)
  }
  if (kind == "insert_select") {
    m <- regexpr(
      "(?i)^INSERT\\s+INTO\\s+([A-Za-z_#\\[][A-Za-z0-9_#.\\]]*)",
      text, perl = TRUE
    )
    if (m > 0L) {
      raw <- regmatches(text, m)
      tbl <- trimws(sub("(?i)^INSERT\\s+INTO\\s+", "", raw, perl = TRUE))
      return(gsub("^\\[|\\]$", "", tbl))
    }
    return(NULL)
  }
  if (kind == "create_table") {
    m <- regexpr(
      "(?i)^CREATE\\s+TABLE\\s+([A-Za-z_#\\[][A-Za-z0-9_#.\\]]*)",
      text, perl = TRUE
    )
    if (m > 0L) {
      raw <- regmatches(text, m)
      tbl <- trimws(sub("(?i)^CREATE\\s+TABLE\\s+", "", raw, perl = TRUE))
      return(gsub("^\\[|\\]$", "", tbl))
    }
    return(NULL)
  }
  NULL
}

# After parsing a SELECT INTO / INSERT SELECT, register the output columns
# from the first output stage into the temp registry.
register_select_output <- function(temp_registry, parsed, output_tbl, origin_seq) {
  for (st in parsed$statements) {
    for (stg in st$stages) {
      if (!identical(stg$role, "output")) next
      cols <- tibble::tibble(
        column  = vapply(stg$projections, function(p) p$output, character(1)),
        type    = NA_character_,
        ordinal = seq_along(stg$projections)
      )
      if (nrow(cols) > 0L) {
        temp_registry <- register_temp_table(temp_registry, output_tbl,
                                             cols, origin_seq)
      }
      return(temp_registry)
    }
  }
  temp_registry
}

# Return a copy of schema restricted to tables whose leaf name appears as a
# word token in the SQL string. This is a deliberate over-approximation: any
# word that matches a table name is kept, so there will be false positives
# (e.g. a column alias that happens to share a table name), but no false
# negatives. The result is used immediately by as_sqlglot_schema() so the
# reticulate payload stays small even against catalogs with thousands of tables.
filter_schema_to_sql <- function(sql, schema) {
  # Extract every word-like token from the SQL (identifiers, keywords, literals).
  # Bracket-quoted names like [my_table] are handled by also matching the inner
  # word; the brackets themselves are non-word characters and split naturally.
  tokens <- tolower(unique(stringr::str_extract_all(sql, "[A-Za-z_#][A-Za-z0-9_#]*")[[1]]))

  keep <- tolower(schema$columns$table) %in% tokens
  if (!any(keep)) {
    # No tables matched — fall back to the full schema rather than passing
    # nothing (which would disable qualification entirely).
    return(schema)
  }
  new_schema(schema$columns[keep, , drop = FALSE])
}

# Convert an rdataflow_schema into the flat mapping sqlglot wants:
# {table: {column: type}}.
#
# sqlglot's MappingSchema requires a *uniform* nesting depth across every
# table (mixing `schema.table` with bare `table` raises a SchemaError), and
# its qualifier resolves columns by leaf table name regardless of how the
# query qualifies the reference. A flat, depth-1 map therefore satisfies the
# uniformity requirement while still resolving both `FROM dbo.customers` and
# `FROM customers`. Trade-off: two same-named tables in different schemas
# would collide; that rare case is accepted for now (last one wins).
as_sqlglot_schema <- function(schema) {
  stopifnot(inherits(schema, "rdataflow_schema"))
  cols <- schema$columns

  out <- list()
  for (tbl in unique(cols$table)) {
    rows <- cols[cols$table == tbl, , drop = FALSE]
    # Build the {column: type} leaf. sqlglot needs a type string, so fall
    # back to "UNKNOWN" when the type is not recorded.
    out[[tbl]] <- as.list(stats::setNames(
      ifelse(is.na(rows$type), "UNKNOWN", rows$type),
      rows$column
    ))
  }
  out
}

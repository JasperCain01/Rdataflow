# ---------------------------------------------------------------------------
# Schema / metadata layer
#
# The "schema object" is the catalog of every column that *could* appear in
# the tables a SQL script references. It is the foundation the rest of the
# pipeline builds on: the parser uses it to expand `*` and qualify columns,
# and the renderer uses it to show the full column list per table (so we can
# highlight which columns are actually used).
#
# A schema object is an S3 object of class "rdataflow_schema" wrapping a
# single tibble with one row per (table, column):
#   catalog  - database name (NA if unspecified)
#   schema   - SQL schema, e.g. "dbo" (NA if unspecified)
#   table    - table / view name
#   column   - column name
#   type     - declared SQL type (NA if unknown)
#   ordinal  - 1-based column position within the table
# ---------------------------------------------------------------------------

#' Construct a schema object from a normalized tibble
#'
#' Internal constructor. Validates the required columns are present and
#' attaches the S3 class. All public constructors funnel through here so the
#' invariant (one row per table/column, with the standard column set) holds
#' everywhere downstream.
#'
#' @param columns A tibble with `catalog`, `schema`, `table`, `column`,
#'   `type`, and `ordinal` columns.
#'
#' @return An object of class `rdataflow_schema`.
#' @keywords internal
new_schema <- function(columns) {
  # Guard the invariant: every schema object carries exactly these fields so
  # downstream code can rely on them without defensive checks.
  required <- c("catalog", "schema", "table", "column", "type", "ordinal")
  stopifnot(tibble::is_tibble(columns), all(required %in% names(columns)))

  structure(
    list(columns = columns[required]),
    class = "rdataflow_schema"
  )
}

#' Build a schema object from a named list
#'
#' This is the offline / testing entry point: it lets you describe table
#' metadata inline without a live database connection. Each list element is a
#' table; its name is the (optionally qualified) table identifier and its
#' value lists the columns.
#'
#' Column values may be supplied two ways:
#' * an **unnamed** character vector of column names (types unknown), or
#' * a **named** character vector mapping column name to SQL type.
#'
#' Table identifiers may be qualified and bracket-quoted in T-SQL style, e.g.
#' `"dbo.customers"`, `"[dbo].[customers]"`, or `"sales.dbo.orders"`.
#'
#' @param tables A named list describing each table's columns (see details).
#'
#' @return An object of class `rdataflow_schema`.
#' @export
#'
#' @examples
#' schema_from_list(list(
#'   "dbo.customers" = c("customer_id", "name", "region_id"),
#'   "dbo.regions"   = c(region_id = "INT", region_name = "VARCHAR(50)")
#' ))
schema_from_list <- function(tables) {
  # A named list is required because the names carry the table identifiers.
  if (!is.list(tables) || is.null(names(tables)) || any(!nzchar(names(tables)))) {
    rlang::abort("`tables` must be a named list of table -> columns.")
  }

  # Expand every table entry into a tibble of one row per column, then stack
  # them. imap passes both the column spec and the table name; list_rbind()
  # combines the per-table tibbles into one.
  columns <- purrr::imap(tables, function(cols, table_id) {
    # Split the (possibly qualified, possibly bracketed) identifier into its
    # catalog / schema / table parts.
    parts <- parse_table_identifier(table_id)

    # Normalise the column spec into parallel name/type vectors. A named
    # vector carries types in its values; an unnamed vector is names only.
    nms <- names(cols)
    if (is.null(nms)) {
      col_names <- as.character(cols)
      col_types <- rep(NA_character_, length(cols))
    } else {
      col_names <- nms
      col_types <- as.character(cols)
    }

    tibble::tibble(
      catalog = parts$catalog,
      schema = parts$schema,
      table = parts$table,
      column = col_names,
      type = col_types,
      # Ordinal position is just the order the columns were given in.
      ordinal = seq_along(col_names)
    )
  }) |> purrr::list_rbind()

  new_schema(columns)
}

#' Build a schema object from a live database connection
#'
#' Reads column metadata from `INFORMATION_SCHEMA.COLUMNS` over a `DBI`
#' connection (typically an `odbc` connection to SQL Server). This is the
#' primary, zero-manual-effort metadata source for real scripts.
#'
#' @param con A `DBI` connection (e.g. created with `odbc::dbConnect()`).
#' @param tables Optional character vector of table identifiers to restrict
#'   the catalog to (matched on table name, optionally `schema.table`). When
#'   `NULL` (default) every table the connection can see is read.
#' @param catalog Optional database/catalog name to filter on.
#'
#' @return An object of class `rdataflow_schema`.
#' @export
schema_from_con <- function(con, tables = NULL, catalog = NULL) {
  # DBI is only needed for the live path, so it is a Suggested dependency we
  # check for at call time rather than forcing on every user.
  if (!requireNamespace("DBI", quietly = TRUE)) {
    rlang::abort("Package 'DBI' is required for schema_from_con().")
  }

  # Pull the standard ANSI INFORMATION_SCHEMA.COLUMNS view, which SQL Server
  # exposes. We select the canonical columns and let the WHERE clause filter
  # down to the catalog / tables of interest.
  query <- paste(
    "SELECT TABLE_CATALOG, TABLE_SCHEMA, TABLE_NAME, COLUMN_NAME,",
    "DATA_TYPE, ORDINAL_POSITION",
    "FROM INFORMATION_SCHEMA.COLUMNS"
  )

  raw <- DBI::dbGetQuery(con, query)

  # Normalise the column names to our internal vocabulary. INFORMATION_SCHEMA
  # returns upper-case names; rename defensively in case a driver lower-cases.
  names(raw) <- toupper(names(raw))
  columns <- tibble::tibble(
    catalog = as.character(raw$TABLE_CATALOG),
    schema = as.character(raw$TABLE_SCHEMA),
    table = as.character(raw$TABLE_NAME),
    column = as.character(raw$COLUMN_NAME),
    type = as.character(raw$DATA_TYPE),
    ordinal = as.integer(raw$ORDINAL_POSITION)
  )

  # Optional catalog filter: keep only rows from the requested database.
  if (!is.null(catalog)) {
    columns <- columns[columns$catalog == catalog, , drop = FALSE]
  }

  # Optional table filter: accept either bare table names or schema.table
  # forms and keep rows whose table (or schema.table) matches. Matching is
  # case-insensitive because SQL Server identifiers are by default.
  if (!is.null(tables)) {
    wanted <- purrr::map(tables, parse_table_identifier)
    keep <- purrr::map_lgl(seq_len(nrow(columns)), function(i) {
      any(purrr::map_lgl(wanted, function(w) {
        table_matches(w, columns$schema[i], columns$table[i])
      }))
    })
    columns <- columns[keep, , drop = FALSE]
  }

  new_schema(columns)
}

# Split a (possibly qualified, possibly bracket-quoted) table identifier into
# catalog / schema / table parts. T-SQL allows up to 4-part names
# (server.database.schema.object); we map the *rightmost* parts to
# table/schema/catalog because those are what column resolution needs, and
# ignore any server prefix beyond the catalog.
parse_table_identifier <- function(id) {
  # Bracket-aware split on ".": a dot inside [...] is part of the identifier
  # (e.g. [my.table]) and must not split.
  parts <- split_dotted_identifier(id)
  # Strip the surrounding [ ] quoting from each part now that we have split.
  parts <- vapply(parts, strip_brackets, character(1))
  n <- length(parts)

  # Assign from the right: last is table, then schema, then catalog.
  list(
    catalog = if (n >= 3) parts[[n - 2]] else NA_character_,
    schema  = if (n >= 2) parts[[n - 1]] else NA_character_,
    table   = parts[[n]]
  )
}

# Split on top-level "." only — dots inside [ ] brackets are ignored so that
# bracket-quoted identifiers containing dots stay intact.
split_dotted_identifier <- function(id) {
  chars <- strsplit(id, "", fixed = TRUE)[[1]]
  parts <- character()
  current <- ""
  in_bracket <- FALSE
  for (ch in chars) {
    if (ch == "[") {
      in_bracket <- TRUE
      current <- paste0(current, ch)
    } else if (ch == "]") {
      in_bracket <- FALSE
      current <- paste0(current, ch)
    } else if (ch == "." && !in_bracket) {
      # Top-level separator: flush the accumulated part.
      parts <- c(parts, current)
      current <- ""
    } else {
      current <- paste0(current, ch)
    }
  }
  c(parts, current)
}

# Remove a single layer of [ ] quoting from an identifier part, if present.
strip_brackets <- function(x) {
  stringr::str_replace(x, "^\\[(.*)\\]$", "\\1")
}

# TRUE if a parsed wanted-identifier matches an actual (schema, table) pair.
# If the wanted identifier specified a schema, both must match; otherwise we
# match on table name alone. Comparison is case-insensitive.
table_matches <- function(wanted, actual_schema, actual_table) {
  table_ok <- tolower(wanted$table) == tolower(actual_table %||% "")
  if (is.na(wanted$schema)) {
    table_ok
  } else {
    table_ok && tolower(wanted$schema) == tolower(actual_schema %||% "")
  }
}

#' @export
print.rdataflow_schema <- function(x, ...) {
  # Summarise the catalog compactly: number of tables and a per-table column
  # count, which is what you usually want to eyeball when checking a schema.
  cols <- x$columns
  n_tables <- length(unique(paste(cols$schema, cols$table)))
  cat(sprintf("<rdataflow_schema: %d tables, %d columns>\n",
              n_tables, nrow(cols)))
  invisible(x)
}

# Local null-coalesce (mirrors rlang's %||%) so this file is self-contained.
`%||%` <- function(x, y) if (is.null(x)) y else x

# ---------------------------------------------------------------------------
# Temp-table schema registry
#
# Tracks column metadata for #temp tables encountered during script processing.
# Populated from:
#   (a) CREATE TABLE #t (col1 TYPE, col2 TYPE, ...) statements
#   (b) SELECT-into stage output columns registered after sqlglot parsing
#
# The registry is an append-only tibble {table, column, type, ordinal,
# origin_seq} consumed by merge_temp_schema() before each SELECT is sent to
# sqlglot, so the qualifier can resolve temp-table columns just like base-table
# columns.
# ---------------------------------------------------------------------------

# Parse the parenthesised column definition list from a CREATE TABLE statement.
# Splits on top-level commas (depth 0), then extracts the first two tokens of
# each definition: column name and data type. Constraint noise (NOT NULL,
# PRIMARY KEY, DEFAULT, ...) is ignored.
#
# Returns a tibble {column, type, ordinal}.
parse_create_table_columns <- function(text) {
  # Extract everything between the first '(' and the matching closing ')'.
  open <- regexpr("(", text, fixed = TRUE)
  if (open == -1L) {
    return(tibble::tibble(
      column  = character(),
      type    = character(),
      ordinal = integer()
    ))
  }

  # Walk forward from the opening paren to find its matching close.
  chars <- strsplit(text, "")[[1]]
  start <- as.integer(open) + 1L
  depth <- 1L
  pos <- start
  n <- length(chars)

  while (pos <= n && depth > 0L) {
    if (chars[pos] == "(") depth <- depth + 1L
    if (chars[pos] == ")") depth <- depth - 1L
    pos <- pos + 1L
  }

  inner <- paste(chars[start:(pos - 2L)], collapse = "")

  # Split inner text on top-level commas.
  parts <- split_on_top_level_commas(inner)

  # Parse each part: first token = column name, second = type base.
  # Column may be bracket-quoted [name with spaces]; type is a plain word.
  # We extract them separately rather than a single regex so we don't fight
  # TRE's POSIX character-class rules (which don't honour \] inside [...]).
  rows <- lapply(seq_along(parts), function(i) {
    part <- trimws(parts[[i]])
    if (nchar(part) == 0L) return(NULL)

    # --- Extract column name ---
    # Try bracket-quoted first: [col name]
    bm <- regexpr("^\\[([^\\]]+)\\]", part, perl = TRUE)
    if (bm > 0L) {
      col_name <- regmatches(part, bm)
      col_name <- substr(col_name, 2L, nchar(col_name) - 1L)  # strip [ ]
      rest <- trimws(substring(part, attr(bm, "match.length") + 1L))
    } else {
      # Plain identifier: letters, digits, underscore, #
      wm <- regexpr("[A-Za-z_#][A-Za-z0-9_#]*", part)
      if (wm < 0L) return(NULL)
      col_name <- regmatches(part, wm)
      rest <- trimws(substring(part, attr(wm, "match.length") + 1L))
    }

    # --- Extract type: first plain word from the remainder ---
    tm <- regexpr("[A-Za-z][A-Za-z0-9]*", rest)
    col_type <- if (tm > 0L) regmatches(rest, tm) else NA_character_

    list(column = col_name, type = toupper(col_type), ordinal = i)
  })

  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) {
    return(tibble::tibble(
      column  = character(),
      type    = character(),
      ordinal = integer()
    ))
  }

  tibble::tibble(
    column  = vapply(rows, `[[`, character(1), "column"),
    type    = vapply(rows, `[[`, character(1), "type"),
    ordinal = vapply(rows, `[[`, integer(1),   "ordinal")
  )
}

# Split a string on commas that are at paren-depth 0. Returns a character
# vector of the pieces.
split_on_top_level_commas <- function(text) {
  chars <- strsplit(text, "")[[1]]
  depth  <- 0L
  pieces <- character()
  start  <- 1L

  for (i in seq_along(chars)) {
    ch <- chars[i]
    if (ch == "(") {
      depth <- depth + 1L
    } else if (ch == ")") {
      depth <- depth - 1L
    } else if (ch == "," && depth == 0L) {
      pieces <- c(pieces, paste(chars[start:(i - 1L)], collapse = ""))
      start  <- i + 1L
    }
  }
  # Last piece (no trailing comma).
  pieces <- c(pieces, paste(chars[start:length(chars)], collapse = ""))
  pieces
}

#' Create an empty temp-table schema registry
#'
#' Returns a zero-row tibble with the schema expected by [register_temp_table()]
#' and [merge_temp_schema()].
#'
#' @return A tibble with columns `table`, `column`, `type`, `ordinal`,
#'   `origin_seq` (all typed but empty).
#' @export
new_temp_registry <- function() {
  tibble::tibble(
    table      = character(),
    column     = character(),
    type       = character(),
    ordinal    = integer(),
    origin_seq = integer()
  )
}

#' Register a temp table's columns in the registry
#'
#' Appends column metadata for `table_name` to `registry`. If columns for that
#' table are already present, the new rows are appended after the existing ones
#' (last-registration wins for downstream merges because `merge_temp_schema()`
#' picks the most recent set of columns).
#'
#' @param registry A tibble as returned by [new_temp_registry()].
#' @param table_name Name of the temp table (e.g. `"#prison_terms"`).
#' @param columns_tbl A tibble with at least `column` and `type` columns and
#'   optionally `ordinal`, as returned by [parse_create_table_columns()].
#' @param origin_seq Integer: the `seq` index of the originating statement.
#'
#' @return The updated registry tibble.
#' @export
register_temp_table <- function(registry, table_name, columns_tbl, origin_seq) {
  if (nrow(columns_tbl) == 0L) return(registry)

  rows <- tibble::tibble(
    table      = table_name,
    column     = columns_tbl$column,
    type       = columns_tbl$type,
    ordinal    = if ("ordinal" %in% names(columns_tbl)) {
      columns_tbl$ordinal
    } else {
      seq_len(nrow(columns_tbl))
    },
    origin_seq = origin_seq
  )
  dplyr::bind_rows(registry, rows)
}

#' Merge temp-table columns into a sqlglot schema object
#'
#' Combines columns from `temp_registry` that match tables referenced in `sql`
#' with an existing rdataflow_schema, returning a new schema that includes both.
#' This allows the sqlglot qualifier to resolve temp-table column references
#' alongside regular catalog columns.
#'
#' Only the *most recently registered* set of columns for each temp table is
#' used (the highest `origin_seq`), so a later CREATE TABLE or SELECT INTO
#' supersedes an earlier one.
#'
#' @param sql The SQL text of the statement about to be parsed.
#' @param schema An `rdataflow_schema` object (may be `NULL`).
#' @param temp_registry A tibble as returned by [new_temp_registry()].
#'
#' @return A merged `rdataflow_schema` object, or `NULL` if both inputs are
#'   empty.
#' @export
merge_temp_schema <- function(sql, schema, temp_registry) {
  if (nrow(temp_registry) == 0L) return(schema)

  # Extract word tokens from SQL to check which tables are referenced.
  tokens <- tolower(unique(
    stringr::str_extract_all(sql, "[A-Za-z_#][A-Za-z0-9_#]*")[[1]]
  ))

  # Filter registry to tables mentioned in this SQL.
  rel <- temp_registry[tolower(temp_registry$table) %in% tokens, , drop = FALSE]
  if (nrow(rel) == 0L) return(schema)

  # For each temp table, keep only the most recently registered columns.
  # .data$ prefix silences the "no visible binding" NOTE from R CMD check.
  latest <- rel |>
    dplyr::group_by(table) |>
    dplyr::filter(.data$origin_seq == max(.data$origin_seq)) |>
    dplyr::ungroup()

  # Build the columns tibble expected by new_schema() — catalog and schema are
  # not meaningful for temp tables so we leave them as NA.
  temp_cols <- tibble::tibble(
    catalog = NA_character_,
    schema  = NA_character_,
    table   = latest$table,
    column  = latest$column,
    type    = latest$type,
    ordinal = latest$ordinal
  )

  if (is.null(schema)) {
    return(new_schema(temp_cols))
  }

  # Merge: remove any existing entries for temp tables (the fresh registration
  # is authoritative), then append the temp columns.
  existing <- schema$columns[
    !tolower(schema$columns$table) %in% tolower(latest$table), , drop = FALSE
  ]
  new_schema(dplyr::bind_rows(existing, temp_cols))
}

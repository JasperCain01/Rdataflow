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
#' @param sql A SQL script as a length-1 character vector.
#' @param schema An optional `rdataflow_schema` object (see
#'   [schema_from_list()] / [schema_from_con()]). When supplied, `*` is
#'   expanded and unqualified columns are resolved to their tables.
#' @param dialect sqlglot dialect name; defaults to `"tsql"`.
#'
#' @return A nested list with a `statements` element. Each statement carries
#'   `index`, `kind`, `output_table`, and a list of `stages`.
#' @export
parse_sql <- function(sql, schema = NULL, dialect = "tsql") {
  stopifnot(is.character(sql), length(sql) == 1)

  mod <- load_py_module()

  # Convert our schema object into the nested mapping sqlglot's qualifier
  # expects; NULL means "no schema" (parse without qualification).
  py_schema <- if (is.null(schema)) NULL else as_sqlglot_schema(schema)

  # reticulate auto-converts the returned Python dict into a nested R list.
  mod$extract_lineage(sql, schema = py_schema, dialect = dialect)
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

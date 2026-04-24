#' Parse an R script into a tibble of assignments
#'
#' Reads an R script (or character vector of code) and returns a tibble with
#' one row per top-level assignment. For each assignment, the output records
#' the target variable, the source code of the right-hand side, the set of
#' variables referenced on the right-hand side, and a classification of the
#' operation (`"import"`, `"transform"`, or `"other"`).
#'
#' Imports are detected by function name (e.g. `read_csv`, `read.csv`,
#' `readRDS`, `read_excel`, ...). Transformations are any other call whose
#' right-hand side references at least one previously-defined variable.
#'
#' @param script Either a path to an R script or a character vector of R
#'   source code.
#'
#' @return A tibble with columns `step`, `target`, `rhs_code`, `call_fn`,
#'   `inputs` (list column of character vectors), and `kind`.
#' @export
parse_script <- function(script) {
  # Read the script from disk (if `script` is a path) or from memory
  # (if it is already a character vector of code), then parse it into
  # a list of top-level R expressions we can inspect programmatically.
  code <- read_script_source(script)
  exprs <- parse(text = code, keep.source = FALSE)

  # Walk each top-level expression with its 1-based position (`step`)
  # and return a per-assignment tibble row; `imap_dfr` row-binds the
  # results and silently drops the empty tibbles returned for any
  # non-assignment expression (library() calls, bare print()s, etc.).
  purrr::imap_dfr(as.list(exprs), function(expr, i) {
    # Only `<-` / `=` / `<<-` to a bare symbol counts as an assignment
    # worth tracing; anything else contributes no node to the graph.
    assignment <- extract_assignment(expr)
    if (is.null(assignment)) {
      # Return a zero-row tibble with the correct column types so the
      # row-bind keeps a stable schema across all expressions.
      return(tibble::tibble(
        step = integer(),
        target = character(),
        rhs_code = character(),
        call_fn = character(),
        inputs = list(),
        kind = character()
      ))
    }

    # Pull out every variable referenced on the RHS and the name of
    # the (outermost) function call — together these determine how
    # the assignment is classified in the flow diagram.
    inputs <- extract_symbols(assignment$rhs)
    call_fn <- extract_call_fn(assignment$rhs)

    # One row per assignment, carrying both machine-readable columns
    # (`inputs`, `call_fn`, `kind`) and the raw RHS source code for
    # display on the diagram / downstream inspection.
    tibble::tibble(
      step = i,
      target = assignment$target,
      rhs_code = paste(deparse(assignment$rhs), collapse = " "),
      call_fn = call_fn %||% NA_character_,
      inputs = list(inputs),
      kind = classify_call(call_fn, inputs)
    )
  })
}

# Accept either a path to a .R file or a character vector of code so
# callers can pass whichever is most convenient. A single-element
# character vector that happens to exist as a file is treated as a path.
read_script_source <- function(script) {
  if (length(script) == 1 && file.exists(script)) {
    paste(readLines(script, warn = FALSE), collapse = "\n")
  } else {
    paste(script, collapse = "\n")
  }
}

# Return `list(target, rhs)` if `expr` is a top-level assignment whose
# LHS is a bare symbol; return NULL for any other expression. We ignore
# assignments to subscripts / attributes because those don't create a
# new named object in the lineage graph.
extract_assignment <- function(expr) {
  if (!is.call(expr)) return(NULL)
  op <- as.character(expr[[1]])
  if (!op %in% c("<-", "=", "<<-")) return(NULL)

  target <- expr[[2]]
  if (!is.symbol(target)) return(NULL)

  list(
    target = as.character(target),
    rhs = expr[[3]]
  )
}

# Recursively collect every symbol appearing in `expr` (excluding the
# function-name slot of each call). These symbols are candidate inputs:
# the graph builder later keeps only those matching previously-defined
# targets, so we can be liberal about what we harvest here.
extract_symbols <- function(expr) {
  syms <- character()
  walk <- function(e) {
    if (is.symbol(e)) {
      # A plain variable reference — record it.
      syms <<- c(syms, as.character(e))
    } else if (is.call(e)) {
      # Recurse into arguments only; skipping slot 1 prevents us from
      # treating function names (e.g. `filter`) as data inputs.
      for (arg in as.list(e)[-1]) {
        if (!missing(arg)) walk(arg)
      }
    }
  }
  walk(expr)
  unique(syms)
}

# Identify the "headline" function applied on the RHS. For piped
# expressions this is the last call in the chain (e.g. the `mutate` in
# `x |> filter(...) |> mutate(...)`), because that is the operation
# that actually produces the assigned value.
extract_call_fn <- function(expr) {
  if (!is.call(expr)) return(NULL)

  # Peel pipes so the function we report is the *deepest* call —
  # e.g. `x |> filter(...) |> mutate(...)` reports "mutate". We also
  # require the head to be a symbol before comparing, because an
  # `lhs |> pkg::fn()` head is itself a `::` call of length 3 and
  # would otherwise break the scalar `&&`.
  pipe_ops <- c("|>", "%>%")
  while (is.call(expr) && length(expr) >= 3 &&
           is.symbol(expr[[1]]) &&
           as.character(expr[[1]]) %in% pipe_ops) {
    expr <- expr[[3]]
  }

  # After peeling, decode the call head. It is either a bare symbol
  # (`filter(...)`) or a namespaced call (`dplyr::filter(...)`), and
  # we want the unqualified function name in both cases.
  if (!is.call(expr)) return(NULL)
  fn <- expr[[1]]
  if (is.symbol(fn)) {
    as.character(fn)
  } else if (is.call(fn) && as.character(fn[[1]]) %in% c("::", ":::")) {
    as.character(fn[[3]])
  } else {
    NULL
  }
}

# Whitelist of function names that read data from an external source.
# An assignment whose headline call is in this list is classified as an
# "import" node and rendered differently in the flow diagram. Extend
# this vector as additional I/O functions are encountered.
import_fns <- c(
  "read.csv", "read.csv2", "read.delim", "read.table", "readRDS",
  "readLines", "load", "scan", "read_csv", "read_csv2", "read_tsv",
  "read_delim", "read_table", "read_fwf", "read_lines", "read_rds",
  "read_excel", "read_xls", "read_xlsx", "read_sav", "read_dta",
  "read_sas", "read_parquet", "read_feather", "fromJSON", "dbGetQuery",
  "dbReadTable", "st_read", "read_sf"
)

# Bucket an assignment into one of three categories used for colouring
# and shaping diagram nodes: a known I/O call, a call that references
# at least one other variable (a transformation of prior data), or an
# "other" case (constants, literal tibbles, etc.).
classify_call <- function(call_fn, inputs) {
  if (!is.null(call_fn) && call_fn %in% import_fns) {
    "import"
  } else if (length(inputs) > 0) {
    "transform"
  } else {
    "other"
  }
}

# Local null-coalesce so we do not force a hard dependency on rlang's
# `%||%` being re-exported. Returns `y` when `x` is NULL, else `x`.
`%||%` <- function(x, y) if (is.null(x)) y else x

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
  code <- read_script_source(script)
  exprs <- parse(text = code, keep.source = FALSE)

  purrr::imap_dfr(as.list(exprs), function(expr, i) {
    assignment <- extract_assignment(expr)
    if (is.null(assignment)) {
      return(tibble::tibble(
        step = integer(),
        target = character(),
        rhs_code = character(),
        call_fn = character(),
        inputs = list(),
        kind = character()
      ))
    }

    inputs <- extract_symbols(assignment$rhs)
    call_fn <- extract_call_fn(assignment$rhs)

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

read_script_source <- function(script) {
  if (length(script) == 1 && file.exists(script)) {
    paste(readLines(script, warn = FALSE), collapse = "\n")
  } else {
    paste(script, collapse = "\n")
  }
}

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

extract_symbols <- function(expr) {
  syms <- character()
  walk <- function(e) {
    if (is.symbol(e)) {
      syms <<- c(syms, as.character(e))
    } else if (is.call(e)) {
      # Skip the function slot for symbol harvesting; named args are dropped.
      for (arg in as.list(e)[-1]) {
        if (!missing(arg)) walk(arg)
      }
    }
  }
  walk(expr)
  unique(syms)
}

extract_call_fn <- function(expr) {
  if (!is.call(expr)) return(NULL)

  # Peel pipes so the function we report is the *deepest* call —
  # e.g. `x |> filter(...) |> mutate(...)` reports "mutate".
  pipe_ops <- c("|>", "%>%")
  while (is.call(expr) && length(expr) >= 3 &&
           is.symbol(expr[[1]]) &&
           as.character(expr[[1]]) %in% pipe_ops) {
    expr <- expr[[3]]
  }

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

import_fns <- c(
  "read.csv", "read.csv2", "read.delim", "read.table", "readRDS",
  "readLines", "load", "scan", "read_csv", "read_csv2", "read_tsv",
  "read_delim", "read_table", "read_fwf", "read_lines", "read_rds",
  "read_excel", "read_xls", "read_xlsx", "read_sav", "read_dta",
  "read_sas", "read_parquet", "read_feather", "fromJSON", "dbGetQuery",
  "dbReadTable", "st_read", "read_sf"
)

classify_call <- function(call_fn, inputs) {
  if (!is.null(call_fn) && call_fn %in% import_fns) {
    "import"
  } else if (length(inputs) > 0) {
    "transform"
  } else {
    "other"
  }
}

`%||%` <- function(x, y) if (is.null(x)) y else x

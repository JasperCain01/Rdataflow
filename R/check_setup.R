# ---------------------------------------------------------------------------
# Setup diagnosis
#
# One function a user on a locked-down machine can run and read (or retype)
# the output of. Exercises every link in the parse chain in order, so the
# first FAILED line names the broken link: reticulate -> Python -> sqlglot
# -> bundled module -> isolated child session -> end-to-end parse.
# ---------------------------------------------------------------------------

#' Diagnose the Python / sqlglot setup
#'
#' Checks each link in the parsing chain in dependency order and prints a
#' `ok` / `FAILED` line per step: is `reticulate` installed, which Python it
#' found, is `sqlglot` importable (and its version), can the bundled
#' `rdataflow_sqlglot.py` module be located, does the isolated `callr`
#' child session parse a trivial statement, and does a tiny script survive
#' the full [parse_sql()] pipeline.
#'
#' The first `FAILED` step is the broken link; later failures are usually
#' just its consequences. Typical fixes: `install.packages("reticulate")`,
#' [install_sqlglot()], reinstalling the package (or setting
#' `RDATAFLOW_PY_PATH`) when the bundled module is missing, and checking
#' `reticulate::py_config()` in a fresh session when only the child fails.
#'
#' @return Invisibly, a named logical vector, one element per check.
#' @export
#'
#' @examples
#' \dontrun{
#' check_setup()
#' }
check_setup <- function() {
  results <- c()
  say <- function(label, ok, detail = "") {
    status <- if (isTRUE(ok)) "ok" else "FAILED"
    pad <- strrep(" ", max(1L, 34L - nchar(label)))
    cat(label, pad, status,
        if (nzchar(detail)) paste0("  (", detail, ")") else "", "\n", sep = "")
    results[[label]] <<- isTRUE(ok)
  }

  # 1. reticulate present
  has_ret <- requireNamespace("reticulate", quietly = TRUE)
  say("reticulate installed", has_ret,
      if (!has_ret) 'install.packages("reticulate")' else "")

  # 2. Python discoverable
  py <- NULL
  if (has_ret) {
    cfg <- tryCatch(reticulate::py_discover_config(), error = function(e) NULL)
    py <- if (!is.null(cfg)) cfg$python else NULL
  }
  say("Python found", !is.null(py) && nzchar(py), py %||% "")

  # 3. sqlglot importable (and version)
  has_sqlglot <- has_ret && sqlglot_available()
  ver <- if (has_sqlglot) {
    tryCatch(reticulate::import("sqlglot")$`__version__`,
             error = function(e) "version unknown")
  } else {
    "run install_sqlglot()"
  }
  say("sqlglot importable", has_sqlglot, ver)

  # 4. bundled Python module locatable
  py_path <- tryCatch(find_py_path(), error = function(e) NULL)
  say("bundled module found", !is.null(py_path), py_path %||%
        "reinstall the package, or set RDATAFLOW_PY_PATH")

  # 5. isolated child session parses a trivial statement
  child_ok <- FALSE
  child_note <- "skipped (earlier step failed)"
  if (has_sqlglot && !is.null(py_path)) {
    iso <- tryCatch(
      parse_one_select_isolated("SELECT 1 AS x", schema = NULL),
      error = function(e) list(result = NULL,
                               skipped_log = conditionMessage(e))
    )
    child_ok <- !is.null(iso$result)
    child_note <- if (child_ok) "" else
      paste(iso$skipped_log, collapse = "; ")
  }
  say("isolated child session works", child_ok, child_note)

  # 6. end-to-end parse of a tiny script
  e2e_ok <- FALSE
  e2e_note <- "skipped (earlier step failed)"
  if (child_ok) {
    p <- tryCatch(
      suppressMessages(suppressWarnings(
        parse_sql("SELECT a, b INTO #t FROM dbo.demo WHERE a > 1")
      )),
      error = function(e) NULL
    )
    e2e_ok <- !is.null(p) && length(p$statements) == 1L
    e2e_note <- if (e2e_ok) "" else "parse_sql() returned no statements"
  }
  say("end-to-end parse works", e2e_ok, e2e_note)

  if (all(unlist(results))) {
    cat("\nAll checks passed - the parsing chain is healthy.\n")
  } else {
    cat("\nFirst FAILED line above is the broken link;",
        "later failures are usually its consequences.\n")
  }
  invisible(unlist(results))
}

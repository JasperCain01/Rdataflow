# ---------------------------------------------------------------------------
# Subprocess isolation for sqlglot parsing
#
# Runs the reticulate → sqlglot extraction inside a persistent callr::r_session
# child process. A C-level crash or segfault in the child is contained: the
# parent R session survives and receives NULL for that statement, which the
# orchestrator logs and skips.
#
# One persistent child is reused for the whole script (not one per statement)
# so the per-statement overhead is a single IPC roundtrip, not a process spawn.
#
# The child does NOT load the Rdataflow package — it only needs reticulate and
# the bundled Python module. The parent converts the schema to a plain R list
# (the format as_sqlglot_schema() produces) before passing it over the pipe,
# avoiding any S3-class serialisation issues.
# ---------------------------------------------------------------------------

# Package-level holder for the persistent callr session.
.isolated_session <- new.env(parent = emptyenv())

# Return the live callr::r_session, creating it if not yet started.
get_isolated_session <- function() {
  sess <- .isolated_session$sess
  if (!is.null(sess) && sess$is_alive()) return(sess)

  # Spawn a new child. Inherit the parent's library paths so the child can
  # load reticulate and any Python env without a separate install.
  sess <- callr::r_session$new(
    options = callr::r_session_options(libpath = .libPaths())
  )
  .isolated_session$sess <- sess
  sess
}

# Reset (kill) the persistent session so the next call spawns a fresh one.
# Called after a crash is detected.
reset_isolated_session <- function() {
  sess <- .isolated_session$sess
  if (!is.null(sess)) {
    try(sess$kill(), silent = TRUE)
  }
  .isolated_session$sess <- NULL
}

#' Parse one SELECT-bearing statement in a subprocess
#'
#' Sends `sql` to a persistent [callr::r_session] child where reticulate and
#' sqlglot perform the extraction. If the child crashes or times out, returns
#' `NULL` and the error is appended to the `skipped_log` in the returned list.
#' The parent R session is always preserved.
#'
#' @param sql A length-1 character vector: the pre-processed SQL for one
#'   SELECT-bearing statement (variables substituted, schema merged).
#' @param schema An optional `rdataflow_schema` object (converted to a plain
#'   list before being passed to the child).
#' @param dialect sqlglot dialect name; default `"tsql"`.
#' @param skipped_log A character vector of previously skipped-statement
#'   messages; the function may append to it.
#' @param timeout_ms Integer milliseconds to wait before giving up on the child.
#'   Default 30 000 (30 s).
#'
#' @return A list with two elements:
#'   - `result`: the nested list returned by sqlglot for this statement, or
#'     `NULL` on failure.
#'   - `skipped_log`: character vector (the input log, possibly extended).
#' @export
parse_one_select_isolated <- function(sql, schema = NULL, dialect = "tsql",
                                      skipped_log = character(),
                                      timeout_ms = 30000L) {
  # Convert schema to a plain R list here in the parent so the child never
  # sees our S3 objects.
  py_schema <- if (is.null(schema)) NULL else as_sqlglot_schema(schema)
  py_path   <- find_py_path()

  sess <- tryCatch(
    get_isolated_session(),
    error = function(e) {
      skipped_log <<- c(
        skipped_log,
        paste0("Could not start isolated session: ", conditionMessage(e))
      )
      NULL
    }
  )

  if (is.null(sess)) {
    return(list(result = NULL, skipped_log = skipped_log))
  }

  # The child only needs reticulate — no Rdataflow library() call required.
  child_fn <- function(sql, py_schema, dialect, py_path) {
    mod <- reticulate::import_from_path("rdataflow_sqlglot", path = py_path)
    mod$extract_lineage(sql, schema = py_schema, dialect = dialect)
  }

  raw <- tryCatch({
    sess$call(child_fn,
              args = list(sql = sql, py_schema = py_schema,
                          dialect = dialect, py_path = py_path))
    # poll_io() blocks until output is ready or timeout_ms elapses.
    # The third element of the returned named vector is the result channel.
    status <- sess$poll_io(timeout_ms)
    if (!identical(unname(status[["process"]]), "ready")) {
      # Always reset on timeout regardless of is_alive(). Returning without
      # calling read() leaves the session's busy flag set, which causes every
      # subsequent sess$call() to throw "R session busy" and cascade failures
      # across all remaining statements.
      reset_isolated_session()
      skipped_log <<- c(skipped_log, "Child timed out")
      return(list(result = NULL, skipped_log = skipped_log))
    }
    sess$read()
  }, error = function(e) {
    msg <- conditionMessage(e)
    # Always reset on any error — the session state is unknown. In particular,
    # "R session busy" (thrown when call() is invoked before the previous
    # result is read) will cascade to every subsequent statement unless we
    # reset here to give the next call a clean session.
    reset_isolated_session()
    skipped_log <<- c(skipped_log, paste0("Child error: ", msg))
    NULL
  })

  # callr wraps results in a callr_session_result; unwrap it.
  if (!is.null(raw) && inherits(raw, "callr_session_result")) {
    if (identical(raw$code, 200L)) {
      return(list(result = raw$result, skipped_log = skipped_log))
    }
    msg <- if (!is.null(raw$error)) conditionMessage(raw$error) else
      paste0("child returned code ", raw$code)
    # Always reset on non-200 result — a failed child may leave partial state.
    reset_isolated_session()
    skipped_log <- c(skipped_log, paste0("Child error: ", msg))
    return(list(result = NULL, skipped_log = skipped_log))
  }

  list(result = raw, skipped_log = skipped_log)
}

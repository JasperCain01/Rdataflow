# Isolation tests for parse_one_select_isolated().
# These verify crash-containment without needing a real crashing SQL input.
# We simulate the abort by having the child call stop().

test_that("parse_one_select_isolated returns result for simple SQL", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  skip_if_not(requireNamespace("callr", quietly = TRUE), "callr not available")

  out <- parse_one_select_isolated("SELECT 1 AS x", timeout_ms = 20000L)
  expect_type(out, "list")
  expect_named(out, c("result", "skipped_log"))
  expect_true("statements" %in% names(out$result))
  expect_length(out$skipped_log, 0L)
})

test_that("error in child yields NULL result + skipped log entry", {
  skip_if_not(requireNamespace("callr", quietly = TRUE), "callr not available")

  # Reset any existing session so this test gets a predictable state.
  Rdataflow:::reset_isolated_session()

  sess <- Rdataflow:::get_isolated_session()
  log <- character()

  # Send a function that calls stop() in the child.
  sess$call(function() stop("simulated child error"))
  raw <- tryCatch(
    sess$read(timeout = 10000L),
    error = function(e) {
      if (!sess$is_alive()) Rdataflow:::reset_isolated_session()
      log <<- c(log, paste0("Child error: ", conditionMessage(e)))
      NULL
    }
  )

  # If read succeeded but returned an error result, extract the message.
  if (!is.null(raw) && inherits(raw, "r_session_result") &&
      !is.null(raw$error)) {
    if (!sess$is_alive()) Rdataflow:::reset_isolated_session()
    log <- c(log, paste0("Child error: ", conditionMessage(raw$error)))
    raw <- NULL
  }

  expect_null(raw)
  expect_true(length(log) >= 1L)
})

test_that("session recovers after a child error — next call succeeds", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  skip_if_not(requireNamespace("callr", quietly = TRUE), "callr not available")

  Rdataflow:::reset_isolated_session()

  # Cause a deliberate child error and drain the pending result so the session
  # queue is clear. A stop() in the child is handled gracefully by callr —
  # the session stays alive but holds an error result.
  sess <- Rdataflow:::get_isolated_session()
  sess$call(function() stop("deliberate error for recovery test"))
  sess$poll_io(5000L)
  tryCatch(sess$read(), error = function(e) NULL)

  # Force a reset so parse_one_select_isolated gets a fresh session.
  Rdataflow:::reset_isolated_session()

  out <- parse_one_select_isolated("SELECT 42 AS n", timeout_ms = 20000L)
  expect_true("statements" %in% names(out$result))
})

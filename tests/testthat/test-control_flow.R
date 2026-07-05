# Control-flow unwrapping (Batch E)

test_that("IF ... BEGIN block yields the inner statement", {
  sql <- "IF @load = 1 BEGIN SELECT a INTO #t FROM dbo.src END"
  stmts <- classify_statements(split_statements(sql))
  out <- unwrap_control_flow(stmts)
  expect_equal(out$statements$kind, "select_into")
  expect_true(any(grepl("wrapper unwrapped", out$notes)))
})

test_that("IF with single statement (no BEGIN) yields the inner statement", {
  sql <- "IF OBJECT_ID('tempdb..#t') IS NOT NULL DROP TABLE #t"
  stmts <- classify_statements(split_statements(sql))
  out <- unwrap_control_flow(stmts)
  expect_equal(out$statements$kind, "drop")
})

test_that("IF EXISTS(SELECT...) condition does not leak the inner SELECT", {
  sql <- "IF EXISTS (SELECT 1 FROM dbo.t) DROP TABLE #x"
  stmts <- classify_statements(split_statements(sql))
  out <- unwrap_control_flow(stmts)
  # the DROP is the governed statement, not the SELECT in the condition
  expect_equal(out$statements$kind, "drop")
  expect_match(out$statements$text, "^DROP")
})

test_that("IF/ELSE with two BEGIN blocks yields both inner statements", {
  sql <- paste(
    "IF @x = 1 BEGIN SELECT a INTO #a FROM dbo.t END",
    "ELSE BEGIN SELECT b INTO #b FROM dbo.u END"
  )
  stmts <- classify_statements(split_statements(sql))
  out <- unwrap_control_flow(stmts)
  expect_equal(nrow(out$statements), 2L)
  expect_true(all(out$statements$kind == "select_into"))
})

test_that("CASE ... END inside a block does not close the block early", {
  sql <- paste(
    "IF @x = 1 BEGIN",
    "SELECT CASE WHEN a > 0 THEN 1 ELSE 0 END AS flag INTO #f FROM dbo.t",
    "END"
  )
  stmts <- classify_statements(split_statements(sql))
  out <- unwrap_control_flow(stmts)
  expect_equal(out$statements$kind, "select_into")
  expect_match(out$statements$text, "CASE WHEN")
})

test_that("BEGIN TRAN is not treated as a block and classifies as transaction", {
  expect_equal(classify_one("BEGIN TRANSACTION"), "transaction")
  expect_equal(classify_one("COMMIT"), "transaction")
  sql <- "BEGIN TRAN; SELECT a INTO #t FROM dbo.src; COMMIT"
  stmts <- classify_statements(split_statements(sql))
  out <- unwrap_control_flow(stmts)
  expect_equal(out$statements$kind, c("transaction", "select_into", "transaction"))
})

test_that("WHILE ... BEGIN block is unwrapped", {
  sql <- "WHILE @i < 10 BEGIN INSERT INTO dbo.log SELECT x FROM dbo.src; SET @i = @i + 1 END"
  stmts <- classify_statements(split_statements(sql))
  out <- unwrap_control_flow(stmts)
  expect_setequal(out$statements$kind, c("insert_select", "set_var"))
})

test_that("parse_sql extracts lineage from inside IF blocks end-to-end", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  sql <- "IF @load = 1 BEGIN SELECT customer_id INTO #c FROM dbo.customers END"
  res <- parse_sql(sql)
  expect_equal(length(res$statements), 1L)
  expect_equal(res$statements[[1]]$kind, "select_into")
  expect_true(any(grepl("wrapper unwrapped", res$skipped)))
})

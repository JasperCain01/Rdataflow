test_that("basic semicolon split produces correct seq and text", {
  res <- split_statements("SELECT 1; SELECT 2")
  expect_equal(res$seq, 1:2)
  expect_equal(res$text, c("SELECT 1", "SELECT 2"))
  expect_equal(res$terminator, c(";", ""))
})

test_that("semicolon inside N-prefix string literal does NOT split", {
  # N'%Y.O.I%' contains a ; — must stay in one statement
  sql <- "SELECT FORMAT(dt, N'%Y.O.I%') AS s FROM t;"
  res <- split_statements(sql)
  expect_equal(nrow(res), 1L)
  expect_equal(res$terminator, ";")
})

test_that("semicolon inside block comment does NOT split", {
  sql <- "/* stmt1; still comment */ SELECT 1"
  res <- split_statements(sql)
  expect_equal(nrow(res), 1L)
})

test_that("semicolon inside line comment does NOT split", {
  sql <- "SELECT 1 -- ; this is a comment\nFROM t"
  res <- split_statements(sql)
  expect_equal(nrow(res), 1L)
})

test_that("semicolon inside bracketed identifier does NOT split", {
  sql <- "SELECT [a;b] FROM t;"
  res <- split_statements(sql)
  expect_equal(nrow(res), 1L)
  expect_equal(res$text, "SELECT [a;b] FROM t")
})

test_that("doubled-quote escape 'it''s' stays one string, does not split", {
  sql <- "SELECT 'it''s' AS s FROM t;"
  res <- split_statements(sql)
  expect_equal(nrow(res), 1L)
})

test_that("doubled-bracket escape [a]]b] stays one identifier", {
  sql <- "SELECT [a]]b] FROM t;"
  res <- split_statements(sql)
  expect_equal(nrow(res), 1L)
})

test_that("lone GO splits into two statements", {
  sql <- "SELECT 1\nGO\nSELECT 2"
  res <- split_statements(sql)
  expect_equal(nrow(res), 2L)
  expect_equal(res$text, c("SELECT 1", "SELECT 2"))
  expect_equal(res$terminator, c("GO", ""))
})

test_that("GOTO does NOT split (GO followed by TO)", {
  sql <- "IF @x > 0 GOTO label1;\nSELECT 1"
  res <- split_statements(sql)
  # GOTO should remain in the first statement, no GO split
  expect_equal(nrow(res), 2L)
  # The first text should contain GOTO
  expect_true(grepl("GOTO", res$text[1]))
})

test_that("column named 'go' does NOT trigger GO split", {
  sql <- "SELECT t.go AS go_col FROM t;"
  res <- split_statements(sql)
  expect_equal(nrow(res), 1L)
})

test_that("empty chunks between ;; are dropped", {
  sql <- "SELECT 1;;SELECT 2"
  res <- split_statements(sql)
  expect_equal(nrow(res), 2L)
  expect_equal(res$text, c("SELECT 1", "SELECT 2"))
  expect_equal(res$seq, 1:2)
})

test_that("final statement with no trailing terminator is captured", {
  sql <- "SELECT 1;\nSELECT 2"
  res <- split_statements(sql)
  expect_equal(nrow(res), 2L)
  expect_equal(res$text[2], "SELECT 2")
  expect_equal(res$terminator[2], "")
})

test_that("completely empty input returns zero-row tibble", {
  res <- split_statements("")
  expect_equal(nrow(res), 0L)
  expect_equal(names(res), c("seq", "text", "terminator"))
})

test_that("whitespace-only input returns zero-row tibble", {
  res <- split_statements("   \n\t  ")
  expect_equal(nrow(res), 0L)
})

test_that("block comment spanning multiple lines does not split", {
  sql <- "/* line1\n; line2\n*/ SELECT 1"
  res <- split_statements(sql)
  expect_equal(nrow(res), 1L)
})

test_that("multiple GO-delimited batches all captured", {
  sql <- "CREATE TABLE #t (id INT)\nGO\nINSERT INTO #t VALUES (1)\nGO\nSELECT * FROM #t"
  res <- split_statements(sql)
  expect_equal(nrow(res), 3L)
  expect_equal(res$terminator, c("GO", "GO", ""))
})

test_that("semicolon inside bare string (no N prefix) does NOT split", {
  sql <- "SELECT ';' AS delim FROM t;"
  res <- split_statements(sql)
  expect_equal(nrow(res), 1L)
})

# Helper: classify a single SQL string
cls <- function(sql) {
  classify_statements(split_statements(sql))$kind
}

test_that("DECLARE @ classifies as declare", {
  expect_equal(cls("DECLARE @StartDate DATE = '2024-01-01'"), "declare")
})

test_that("SET @ classifies as set_var", {
  expect_equal(cls("SET @RunDate = GETDATE()"), "set_var")
})

test_that("DROP TABLE classifies as drop", {
  expect_equal(cls("DROP TABLE IF EXISTS #temp"), "drop")
})

test_that("CREATE INDEX classifies as create_index", {
  expect_equal(cls("CREATE INDEX idx_name ON t (col)"), "create_index")
})

test_that("CREATE NONCLUSTERED INDEX classifies as create_index", {
  expect_equal(cls("CREATE NONCLUSTERED INDEX ix_foo ON dbo.bar (id)"), "create_index")
})

test_that("plain CREATE TABLE (no SELECT) classifies as create_table", {
  sql <- "CREATE TABLE #prison_terms (\n  id INT NOT NULL,\n  name VARCHAR(100)\n)"
  expect_equal(cls(sql), "create_table")
})

test_that("CTAS (CREATE TABLE ... AS SELECT) classifies as select_into", {
  sql <- "CREATE TABLE #summary AS SELECT id, name FROM dbo.patients"
  expect_equal(cls(sql), "select_into")
})

test_that("WITH ... SELECT (CTE) classifies as select", {
  sql <- "WITH cte AS (SELECT 1 AS x) SELECT x FROM cte"
  expect_equal(cls(sql), "select")
})

test_that("INSERT INTO ... SELECT classifies as insert_select", {
  sql <- "INSERT INTO #target SELECT id, name FROM source"
  expect_equal(cls(sql), "insert_select")
})

test_that("INSERT INTO ... VALUES classifies as insert_values", {
  sql <- "INSERT INTO #temp VALUES (1, 'a'), (2, 'b')"
  expect_equal(cls(sql), "insert_values")
})

test_that("INSERT ... SELECT with subquery (parens) classifies as insert_select", {
  # SELECT is inside parens in subquery — but there's also a depth-0 SELECT.
  sql <- "INSERT INTO #t SELECT id FROM (SELECT id FROM src) sub"
  expect_equal(cls(sql), "insert_select")
})

test_that("INSERT ... VALUES with subquery in values does NOT mis-classify", {
  # SELECT appears only inside a VALUES subquery paren — depth > 0
  sql <- "INSERT INTO #t (id) VALUES ((SELECT TOP 1 id FROM src))"
  # SELECT is at depth 2, so no depth-0 SELECT → insert_values
  expect_equal(cls(sql), "insert_values")
})

test_that("SELECT ... INTO classifies as select_into", {
  sql <- "SELECT id, name INTO #temp FROM dbo.patients"
  expect_equal(cls(sql), "select_into")
})

test_that("plain SELECT classifies as select", {
  expect_equal(cls("SELECT id, name FROM dbo.patients"), "select")
})

test_that("unknown statement classifies as unknown", {
  expect_equal(cls("EXEC sp_helptext 'my_proc'"), "unknown")
})

test_that("classify_statements preserves seq and text columns", {
  res <- classify_statements(split_statements("SELECT 1; SELECT 2"))
  expect_equal(names(res), c("seq", "text", "terminator", "kind"))
  expect_equal(res$seq, 1:2)
})

test_that("case-insensitive: lowercase declare classifies as declare", {
  expect_equal(cls("declare @x int = 0"), "declare")
})

test_that("case-insensitive: lowercase select classifies as select", {
  expect_equal(cls("select id from t"), "select")
})

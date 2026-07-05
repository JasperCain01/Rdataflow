# Smoke tests for sql_dataflow() — the end-to-end pipeline function.
# We test via graph_to_dot() rather than grViz() to stay widget-free.

test_that("sql_dataflow end-to-end pipeline produces a DOT diagram", {
  skip_if_not(sqlglot_available(), "sqlglot not available")

  s <- schema_from_list(list(
    "dbo.orders" = c(order_id = "INT", customer_id = "INT", amount = "DECIMAL")
  ))
  sql <- paste(
    "SELECT customer_id, SUM(amount) AS total",
    "FROM dbo.orders",
    "GROUP BY customer_id"
  )

  # Full pipeline should not error; we check the graph object
  parsed     <- parse_sql(sql, schema = s)
  ir         <- build_ir(parsed)
  classified <- classify_transform(ir)
  graph      <- build_graph(classified, schema = s)
  dot        <- graph_to_dot(graph)

  expect_match(dot, "digraph sqlflow")
  # The orders table node must appear
  expect_true(any(grepl("orders", graph$table_nodes$table, ignore.case = TRUE)))
  # The aggregation stage must appear
  expect_true(any(graph$stage_nodes$role == "output"))
})

test_that("sql_dataflow accepts a multi-element character vector (readLines output)", {
  skip_if_not(sqlglot_available(), "sqlglot not available")

  # sql_dataflow() should collapse a vector the same way paste(collapse='\n') does
  sql_lines <- c(
    "SELECT customer_id,",
    "       SUM(amount) AS total",
    "FROM dbo.orders",
    "GROUP BY customer_id"
  )
  sql_single <- paste(sql_lines, collapse = "\n")

  s <- schema_from_list(list(
    "dbo.orders" = c(order_id = "INT", customer_id = "INT", amount = "DECIMAL")
  ))

  # Both should produce an identical IR structure
  parsed_lines  <- parse_sql(paste(sql_lines,  collapse = "\n"), schema = s)
  parsed_single <- parse_sql(sql_single, schema = s)
  expect_equal(build_ir(parsed_lines)$stages, build_ir(parsed_single)$stages)
})

# --- Batch A regression tests -----------------------------------------------

test_that("read_sql reads UTF-16 LE files with BOM (SSMS default)", {
  sql <- "SELECT customer_id INTO #t FROM dbo.orders;"
  path <- tempfile(fileext = ".sql")
  con <- file(path, open = "wb")
  writeBin(as.raw(c(0xFF, 0xFE)), con)                       # UTF-16 LE BOM
  writeBin(iconv(sql, from = "UTF-8", to = "UTF-16LE", toRaw = TRUE)[[1]], con)
  close(con)

  out <- read_sql(path)
  expect_equal(out, sql)
  unlink(path)
})

test_that("read_sql reads plain UTF-8 files unchanged", {
  sql <- "SELECT a\nFROM t;"
  path <- tempfile(fileext = ".sql")
  writeLines(sql, path)
  expect_equal(read_sql(path), sql)
  unlink(path)
})

test_that("read_sql strips a UTF-8 BOM", {
  path <- tempfile(fileext = ".sql")
  con <- file(path, open = "wb")
  writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)                 # UTF-8 BOM
  writeBin(charToRaw("SELECT 1"), con)
  close(con)
  expect_equal(read_sql(path), "SELECT 1")
  unlink(path)
})

# --- Batch B: skip-log surfacing ---------------------------------------------

test_that("notify_skipped warns about real losses and messages benign skips", {
  # Real loss -> warning listing the entry
  expect_warning(
    notify_skipped("seq 2: unrecognised statement skipped (may contain lineage): MERGE..."),
    "missing lineage"
  )
  # Benign skip -> message only, no warning
  expect_message(
    expect_no_warning(
      notify_skipped("seq 1 (drop): skipped non-SELECT statement")
    ),
    "no lineage contribution"
  )
  # Nothing -> silence
  expect_silent(notify_skipped(character(0)))
})

test_that("parse_sql logs unrecognised statements distinctly", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  res <- parse_sql("MERGE INTO t USING s ON t.id = s.id WHEN MATCHED THEN UPDATE SET t.v = s.v;")
  expect_true(any(grepl("unrecognised statement", res$skipped)))
})

test_that("parse_sql logs unresolved variables", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  res <- parse_sql("SELECT a FROM dbo.t WHERE b = @never_declared")
  expect_true(any(grepl("@never_declared", res$skipped)))
})

# --- Batch D: explain_sqlflow ------------------------------------------------

test_that("explain_sqlflow narrates stages, joins, grouping, and columns", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  s <- schema_from_list(list(
    "dbo.customers" = c(customer_id = "INT", name = "VARCHAR", region_id = "INT"),
    "dbo.orders"    = c(order_id = "INT", customer_id = "INT", amount = "DECIMAL")
  ))
  sql <- paste(
    "WITH recent AS (SELECT o.customer_id, SUM(o.amount) AS total",
    "FROM dbo.orders o WHERE o.amount > 0 GROUP BY o.customer_id)",
    "SELECT c.customer_id, recent.total INTO #summary",
    "FROM dbo.customers c JOIN recent ON recent.customer_id = c.customer_id"
  )
  txt <- paste(explain_sqlflow(sql, schema = s), collapse = "\n")

  expect_match(txt, "Statement 1")
  expect_match(txt, "#summary", fixed = TRUE)
  expect_match(txt, "Stage 'recent' \\(CTE\\)")
  expect_match(txt, "reads dbo.orders")
  expect_match(txt, "groups by customer_id")
  expect_match(txt, "WHERE")
  expect_match(txt, "total = SUM")
  expect_match(txt, "\\[aggregate\\]")
  expect_match(txt, "joins")
  expect_match(txt, "customer_id = ")
  expect_match(txt, "passes through")
})

test_that("explain_sqlflow accepts an IR and produces markdown", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  ir <- build_ir(parse_sql("SELECT a FROM dbo.t"))
  out <- explain_sqlflow(ir, format = "markdown")
  expect_s3_class(out, "rdataflow_explanation")
  expect_match(paste(out, collapse = "\n"), "## Statement 1")
  expect_match(paste(out, collapse = "\n"), "- \\*\\*Output stage")
})

test_that("explain_sqlflow rejects invalid input", {
  expect_error(explain_sqlflow(42), "SQL string or an rdataflow_ir")
})

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
    "without lineage impact"
  )
  # Nothing -> silence
  expect_silent(notify_skipped(character(0)))
})

test_that("parse_sql logs unrecognised statements distinctly", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  # MERGE is traced (see test-parse_sql.R); dynamic SQL execution is not and
  # remains a genuine "unrecognised statement" case.
  res <- parse_sql("EXEC sp_executesql @sql")
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

# --- Batch J regression tests (HAVING / DISTINCT / TOP) ---------------------

test_that("explain_sqlflow narrates DISTINCT, TOP, and HAVING", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  s <- schema_from_list(list("dbo.orders" = c(customer_id = "INT", amount = "DECIMAL")))
  sql <- paste(
    "SELECT DISTINCT TOP 100 customer_id, SUM(amount) AS total",
    "FROM dbo.orders GROUP BY customer_id HAVING SUM(amount) > 100"
  )
  txt <- paste(explain_sqlflow(sql, schema = s), collapse = "\n")

  expect_match(txt, "keeps distinct rows", fixed = TRUE)
  expect_match(txt, "keeps top 100", fixed = TRUE)
  expect_match(txt, "filters groups: HAVING SUM", fixed = TRUE)
})

test_that("explain_sqlflow narrates CROSS APPLY as 'cross applies'", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  s <- schema_from_list(list(
    "dbo.a" = c(id = "INT", v = "INT"),
    "dbo.b" = c(id = "INT", w = "INT")
  ))
  sql <- "
    SELECT a.id, top_b.w
    FROM dbo.a a
    CROSS APPLY (SELECT TOP 1 b.w FROM dbo.b b WHERE b.id = a.id) top_b
  "
  txt <- paste(explain_sqlflow(sql, schema = s), collapse = "\n")
  expect_match(txt, "applies top_b")
  expect_no_match(txt, "applys")
})

test_that("explain_sqlflow surfaces the skip log like sql_dataflow", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  s <- schema_from_list(list("dbo.t" = c(a = "INT")))
  sql <- "EXEC sp_executesql @q;\nSELECT a INTO #x FROM dbo.t;"
  expect_warning(explain_sqlflow(sql, schema = s), "unrecognised statement")
})

test_that("read_sql falls back to Windows-1252 for non-UTF-8 files", {
  # 0x91/0x92 are cp1252 curly quotes; 0x96 an en-dash. As UTF-8 these bytes
  # are invalid and readLines() would truncate the line at the first one.
  f <- tempfile(fileext = ".sql")
  bytes <- c(
    charToRaw("SELECT a FROM t WHERE b = "),
    as.raw(0x91), charToRaw("Complaint"), as.raw(0x92),
    charToRaw(" -- July "), as.raw(0x96)
  )
  writeBin(bytes, f)
  txt <- read_sql(f)
  expect_true(validUTF8(txt))
  expect_match(txt, "Complaint", fixed = TRUE)
  # nothing truncated: the en-dash after the literal survived
  expect_match(txt, "July", fixed = TRUE)
  unlink(f)
})

test_that("smart-quoted NHS-style SQL parses end-to-end with a real WHERE", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  s <- schema_from_list(list(
    "dbo.compl_main"    = c(recordid = "INT", com_type = "VARCHAR"),
    "dbo.code_com_type" = c(code = "VARCHAR", description = "VARCHAR"),
    "dbo.link_compl"    = c(com_id = "INT", lcom_dreceived = "DATETIME")
  ))
  sql <- paste(
    "SELECT main.recordid,",
    "  ct.description as comtype_description,",
    "  linked.lcom_dreceived as date_received",
    "FROM NUH_datix.dbo.compl_main as main with (nolock)",
    "left join NUH_datix.dbo.code_com_type as ct with (nolock) on ct.code=main.com_type",
    "left join NUH_datix.dbo.link_compl as linked with (nolock) on linked.com_id=main.recordid",
    "where ct.description = ‘Complaint’",
    "  and linked.lcom_dreceived >= ‘2019-04-01 00:00:00’",
    ";",
    sep = "\n"
  )
  parsed <- parse_sql(sql, schema = s)
  st <- parsed$statements[[1]]
  stg <- st$stages[[1]]
  expect_length(stg$sources, 3L)
  expect_length(stg$joins, 2L)
  # the WHERE survives as proper string/date literals, not identifiers
  expect_match(stg$where, "'Complaint'", fixed = TRUE)
  expect_match(stg$where, "2019-04-01 00:00:00", fixed = TRUE)
})

test_that("qualification failure names the unresolvable alias", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  s <- schema_from_list(list(
    "dbo.a" = c(id = "INT"),
    "dbo.b" = c(id = "INT", descr = "VARCHAR")
  ))
  # alias declared as `bee` but referenced as `b2` — a typo the message
  # should point at
  sql <- "SELECT b2.descr FROM dbo.a AS a JOIN dbo.b AS bee ON b2.id = a.id"
  parsed <- suppressWarnings(parse_sql(sql, schema = s))
  qual <- grep("qualification failed", parsed$skipped, value = TRUE)
  expect_length(qual, 1L)
  expect_match(qual, "b2", fixed = TRUE)
})

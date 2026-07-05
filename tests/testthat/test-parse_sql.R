# These tests exercise the live sqlglot path and are skipped when the Python
# dependency is unavailable (e.g. CI without a configured Python).

test_schema <- function() {
  schema_from_list(list(
    "dbo.customers" = c(customer_id = "INT", name = "VARCHAR", region_id = "INT"),
    "dbo.orders" = c(order_id = "INT", customer_id = "INT", amount = "DECIMAL")
  ))
}

test_that("parse_sql returns stages for a CTE + SELECT INTO", {
  skip_if_not(sqlglot_available(), "sqlglot not available")

  sql <- paste(
    "WITH recent AS (SELECT o.customer_id, SUM(o.amount) AS total",
    "FROM dbo.orders o GROUP BY o.customer_id)",
    "SELECT c.customer_id, recent.total INTO #summary",
    "FROM dbo.customers c JOIN recent ON recent.customer_id = c.customer_id"
  )

  res <- parse_sql(sql, schema = test_schema())
  st <- res$statements[[1]]

  expect_equal(st$kind, "select_into")
  expect_equal(length(st$stages), 2)
  roles <- vapply(st$stages, function(s) s$role, character(1))
  expect_setequal(roles, c("cte", "output"))
})

test_that("parse_sql expands * against the schema", {
  skip_if_not(sqlglot_available(), "sqlglot not available")

  res <- parse_sql("SELECT * FROM dbo.orders", schema = test_schema())
  outputs <- vapply(res$statements[[1]]$stages[[1]]$projections,
                    function(p) p$output, character(1))
  expect_setequal(outputs, c("order_id", "customer_id", "amount"))
})

# --- Batch A regression tests -----------------------------------------------

test_that("extract_output_table handles bracket-quoted identifiers", {
  expect_equal(
    extract_output_table("SELECT a INTO [dbo].[summary] FROM t", "select_into"),
    "dbo.summary"
  )
  expect_equal(
    extract_output_table("SELECT a INTO #tmp FROM t", "select_into"),
    "#tmp"
  )
  expect_equal(
    extract_output_table("INSERT INTO [dbo].[target] (a) SELECT a FROM t",
                         "insert_select"),
    "dbo.target"
  )
  expect_equal(
    extract_output_table("CREATE TABLE [dbo].[t2] (id INT)", "create_table"),
    "dbo.t2"
  )
})

# --- Batch E regression tests -----------------------------------------------

test_that("derived-table subqueries become their own stages", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  sql <- paste(
    "SELECT t.x, d.n FROM dbo.t",
    "JOIN (SELECT id, COUNT(*) AS n FROM dbo.u GROUP BY id) d ON d.id = t.id"
  )
  res <- parse_sql(sql)
  stages <- res$statements[[1]]$stages
  roles <- vapply(stages, function(s) s$role, character(1))
  expect_true("subquery" %in% roles)

  sub <- stages[[which(roles == "subquery")]]
  expect_equal(sub$name, "d")
  # inner table belongs to the subquery stage, not the outer stage
  expect_equal(vapply(sub$sources, function(s) s$table, character(1)), "u")
  out <- stages[[which(roles == "output")]]
  outer_tables <- vapply(out$sources, function(s) s$table, character(1))
  expect_setequal(outer_tables, c("t", "d"))
})

test_that("UNION extracts every branch as an output stage", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  res <- parse_sql("SELECT a FROM dbo.t UNION ALL SELECT a FROM dbo.u")
  stages <- res$statements[[1]]$stages
  expect_equal(length(stages), 2L)
  tabs <- vapply(stages, function(s) s$sources[[1]]$table, character(1))
  expect_setequal(tabs, c("t", "u"))
})

test_that("INSERT column list renames the output columns", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  res <- parse_sql("INSERT INTO dbo.tgt (c1, c2) SELECT a, b FROM dbo.src")
  proj <- res$statements[[1]]$stages[[1]]$projections
  outs <- vapply(proj, function(p) p$output, character(1))
  expect_equal(outs, c("c1", "c2"))
})

test_that("CROSS APPLY becomes a subquery stage with an APPLY join", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  sql <- "SELECT t.x, ca.v FROM dbo.t CROSS APPLY (SELECT v FROM dbo.u WHERE u.id = t.id) ca"
  res <- parse_sql(sql)
  stages <- res$statements[[1]]$stages
  roles <- vapply(stages, function(s) s$role, character(1))
  expect_true("subquery" %in% roles)
  out <- stages[[which(roles == "output")]]
  expect_equal(out$joins[[1]]$kind, "APPLY")
})

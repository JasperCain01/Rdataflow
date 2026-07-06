test_ir <- function() {
  s <- schema_from_list(list(
    "dbo.customers" = c(customer_id = "INT", name = "VARCHAR", region_id = "INT"),
    "dbo.orders" = c(order_id = "INT", customer_id = "INT", amount = "DECIMAL"),
    "dbo.regions" = c(region_id = "INT", region_name = "VARCHAR")
  ))
  sql <- paste(
    "WITH recent AS (SELECT o.customer_id, SUM(o.amount) AS total",
    "FROM dbo.orders o GROUP BY o.customer_id)",
    "SELECT c.customer_id, r.region_name, recent.total INTO #summary",
    "FROM dbo.customers c",
    "JOIN recent ON recent.customer_id = c.customer_id",
    "LEFT JOIN dbo.regions r ON r.region_id = c.region_id"
  )
  build_ir(parse_sql(sql, schema = s))
}

test_that("build_ir produces one row per stage", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  ir <- test_ir()
  expect_s3_class(ir, "rdataflow_ir")
  expect_equal(nrow(ir$stages), 2)
  expect_true(any(ir$stages$is_output))
})

test_that("build_ir resolves source aliases to real tables", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  ir <- test_ir()
  # The CTE aggregates dbo.orders; its total column must trace to orders.amount.
  edge <- ir$proj_sources[ir$proj_sources$output == "total" &
                            ir$proj_sources$src_column == "amount", ]
  expect_equal(edge$src_table, "orders")
})

test_that("build_ir captures joins, keys, and group bys", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  ir <- test_ir()
  expect_equal(nrow(ir$joins), 2)
  expect_true("LEFT" %in% ir$joins$side)
  expect_equal(nrow(ir$join_keys), 2)
  expect_equal(nrow(ir$group_by), 1)
})

# --- Batch H regression tests (indirect lineage: condition/partition/filter) -

test_that("proj_sources carries a role column defaulting to 'value'", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  ir <- test_ir()
  expect_true("role" %in% names(ir$proj_sources))
  expect_true(all(ir$proj_sources$role == "value"))
})

test_that("proj_sources tags CASE WHEN / window columns with their role", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  s <- schema_from_list(list(
    "dbo.t" = c(status = "INT", amount = "DECIMAL", x = "INT",
               grp = "INT", d = "DATE")
  ))
  sql <- paste(
    "SELECT CASE WHEN status = 1 THEN amount ELSE 0 END AS adj,",
    "SUM(x) OVER (PARTITION BY grp ORDER BY d) AS running FROM dbo.t"
  )
  ir <- build_ir(parse_sql(sql, schema = s))
  ps <- ir$proj_sources

  expect_equal(ps$role[ps$src_column == "status"], "condition")
  expect_equal(ps$role[ps$src_column == "amount"], "value")
  expect_equal(ps$role[ps$src_column == "x"], "value")
  expect_equal(ps$role[ps$src_column == "grp"], "partition")
  expect_equal(ps$role[ps$src_column == "d"], "partition")
})

test_that("build_ir captures WHERE-clause columns in the filters tibble", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  s <- schema_from_list(list("dbo.t" = c(a = "INT", b = "INT")))
  ir <- build_ir(parse_sql("SELECT a FROM dbo.t WHERE b > 5", schema = s))
  expect_equal(nrow(ir$filters), 1)
  expect_equal(ir$filters$src_column, "b")
  expect_equal(ir$filters$src_table, "t")
})

test_that("build_ir returns an empty typed filters tibble when there is no WHERE", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  ir <- test_ir()
  expect_equal(nrow(ir$filters), 0)
  expect_setequal(names(ir$filters), c("stage_id", "src_alias", "src_table", "src_column"))
})

# --- Batch J regression tests (HAVING / DISTINCT / TOP) ---------------------

test_that("build_ir captures HAVING, DISTINCT, and TOP on the stage row", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  s <- schema_from_list(list("dbo.orders" = c(customer_id = "INT", amount = "DECIMAL")))
  sql <- paste(
    "SELECT DISTINCT TOP 100 customer_id, SUM(amount) AS total",
    "FROM dbo.orders GROUP BY customer_id HAVING SUM(amount) > 100"
  )
  ir <- build_ir(parse_sql(sql, schema = s))
  expect_true(ir$stages$distinct)
  expect_equal(ir$stages$top, "100")
  expect_match(ir$stages$having, "SUM.*> 100")
})

test_that("build_ir defaults having/distinct/top when absent", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  ir <- test_ir()
  expect_true(all(is.na(ir$stages$having)))
  expect_true(all(!ir$stages$distinct))
  expect_true(all(is.na(ir$stages$top)))
})

test_that("build_ir captures TOP n PERCENT verbatim", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  s <- schema_from_list(list("dbo.t" = c(a = "INT")))
  ir <- build_ir(parse_sql("SELECT TOP 10 PERCENT a FROM dbo.t", schema = s))
  expect_equal(ir$stages$top, "10 PERCENT")
})

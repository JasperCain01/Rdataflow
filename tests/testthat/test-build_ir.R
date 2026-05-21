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

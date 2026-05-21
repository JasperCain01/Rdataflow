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

classified_ir <- function() {
  s <- schema_from_list(list(
    "dbo.customers" = c(customer_id = "INT", name = "VARCHAR", region_id = "INT"),
    "dbo.orders" = c(order_id = "INT", customer_id = "INT", amount = "DECIMAL")
  ))
  sql <- paste(
    "WITH recent AS (SELECT o.customer_id, SUM(o.amount) AS total, COUNT(*) AS n",
    "FROM dbo.orders o GROUP BY o.customer_id)",
    "SELECT c.customer_id,",
    "DATEDIFF(day, '2024-01-01', GETDATE()) AS days_since,",
    "CASE WHEN recent.total > 100 THEN 'big' ELSE 'small' END AS bucket,",
    "c.region_id * 2 AS doubled",
    "INTO #summary",
    "FROM dbo.customers c JOIN recent ON recent.customer_id = c.customer_id"
  )
  classify_transform(build_ir(parse_sql(sql, schema = s)))
}

test_that("projections are categorised by transformation type", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  ir <- classified_ir()
  type_of <- function(out) {
    ir$projections$transform_type[ir$projections$output == out][[1]]
  }
  expect_equal(type_of("total"), "aggregate")
  expect_equal(type_of("n"), "aggregate")
  expect_equal(type_of("days_since"), "date")
  expect_equal(type_of("bucket"), "case")
  expect_equal(type_of("doubled"), "arithmetic")
  expect_equal(type_of("customer_id"), "passthrough")
})

test_that("stage labels summarise grouping and functions", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  ir <- classified_ir()
  st <- ir$stage_transforms

  cte <- st[st$stage_id == 1, ]
  expect_true(cte$has_aggregation)
  expect_match(cte$label, "GROUP BY customer_id")
  expect_match(cte$label, "SUM")
  expect_match(cte$label, "COUNT")

  out <- st[st$stage_id == 2, ]
  expect_true(out$has_date)
  expect_true(out$has_case)
})

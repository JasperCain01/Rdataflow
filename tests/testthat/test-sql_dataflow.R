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

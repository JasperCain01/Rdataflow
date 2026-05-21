# These tests exercise graph_to_dot() (the pure DOT generator) which is
# straightforward to assert against without rendering a widget. plot_sqlflow()
# itself just wraps graph_to_dot() with DiagrammeR::grViz() and is not
# exercised here to avoid requiring DiagrammeR in the test environment.

# Shared fixture: build_graph of the same 3-table CTE query used elsewhere.
test_plot_ir <- function() {
  s <- schema_from_list(list(
    "dbo.customers" = c(customer_id = "INT", name = "VARCHAR", region_id = "INT"),
    "dbo.orders"    = c(order_id = "INT", customer_id = "INT", amount = "DECIMAL"),
    "dbo.regions"   = c(region_id = "INT", region_name = "VARCHAR")
  ))
  sql <- paste(
    "WITH recent AS (SELECT o.customer_id, SUM(o.amount) AS total",
    "FROM dbo.orders o GROUP BY o.customer_id)",
    "SELECT c.customer_id, r.region_name, recent.total INTO #summary",
    "FROM dbo.customers c",
    "JOIN recent ON recent.customer_id = c.customer_id",
    "LEFT JOIN dbo.regions r ON r.region_id = c.region_id"
  )
  build_graph(classify_transform(build_ir(parse_sql(sql, schema = s))), schema = s)
}

test_that("graph_to_dot returns a non-empty string", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  g   <- test_plot_ir()
  dot <- graph_to_dot(g)
  expect_type(dot, "character")
  expect_true(nzchar(dot))
  # Must open and close as a digraph
  expect_match(dot, "digraph sqlflow")
  expect_match(dot, "\\}")
})

test_that("graph_to_dot col-edge mode embeds table and stage node IDs", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  g   <- test_plot_ir()
  dot <- graph_to_dot(g, show_col_edges = TRUE)

  # All node IDs from the graph must appear in the DOT output
  for (nid in c(g$table_nodes$node_id, g$stage_nodes$node_id)) {
    expect_true(grepl(nid, dot, fixed = TRUE),
                info = paste("Missing node ID:", nid))
  }
})

test_that("graph_to_dot col-edge mode contains port-to-port edges", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  g   <- test_plot_ir()
  dot <- graph_to_dot(g, show_col_edges = TRUE)
  # Dashed colour-coded column-level edges use -> with port notation (node:port)
  expect_match(dot, "style=dashed")
  # The orders→recent:total lineage edge must appear
  orders_id <- g$table_nodes$node_id[g$table_nodes$table == "orders"]
  recent_id <- g$stage_nodes$node_id[!is.na(g$stage_nodes$name) & g$stage_nodes$name == "recent"]
  expect_true(grepl(sprintf("%s:amount", orders_id), dot, fixed = TRUE))
  expect_true(grepl(sprintf("%s:total",  recent_id), dot, fixed = TRUE))
})

test_that("graph_to_dot structural mode shows join-type labels", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  g   <- test_plot_ir()
  dot <- graph_to_dot(g, show_col_edges = FALSE)
  # LEFT JOIN should appear as an edge label
  expect_match(dot, "LEFT JOIN")
  # CTE edge label
  expect_match(dot, "CTE")
  # Structural mode has no blue col-edge dashes (col edges use #4a90d9)
  expect_false(grepl("#4a90d9", dot, fixed = TRUE))
})

test_that("graph_to_dot HTML-escapes special characters in labels", {
  col_tbl <- tibble::tibble(
    col_name = "col&<>\"", col_type = "VARCHAR",
    used = TRUE, is_key = FALSE
  )
  # html_esc is sourced into the global env by the test harness
  expect_equal(html_esc("col&<>\""), "col&amp;&lt;&gt;&quot;")

  # html_table_label must not contain raw & in the output
  lbl <- html_table_label("a&b", col_tbl)
  expect_false(grepl("a&b", lbl, fixed = TRUE))
  expect_true(grepl("a&amp;b", lbl, fixed = TRUE))
})

test_that("port_id normalises column names to valid Graphviz identifiers", {
  expect_equal(port_id("order_id"),    "order_id")
  expect_equal(port_id("order.id"),    "order_id")
  expect_equal(port_id("my col"),      "my_col")
  expect_equal(port_id("[bracketed]"), "_bracketed_")
})

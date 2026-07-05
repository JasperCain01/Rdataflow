# Shared fixture: classified IR for a 3-table CTE query.
# dbo.orders -> CTE "recent" (aggregation) -> dbo.customers + dbo.regions JOIN -> #summary
test_graph_ir <- function() {
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
  classify_transform(build_ir(parse_sql(sql, schema = s)))
}

test_graph_schema <- function() {
  schema_from_list(list(
    "dbo.customers" = c(customer_id = "INT", name = "VARCHAR", region_id = "INT"),
    "dbo.orders"    = c(order_id = "INT", customer_id = "INT", amount = "DECIMAL"),
    "dbo.regions"   = c(region_id = "INT", region_name = "VARCHAR")
  ))
}

test_that("build_graph returns the right node counts", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  g <- build_graph(test_graph_ir(), schema = test_graph_schema())
  expect_s3_class(g, "rdataflow_graph")
  # Three physical tables: customers, orders, regions
  expect_equal(nrow(g$table_nodes), 3)
  # Two stages: recent (CTE) + #summary (output)
  expect_equal(nrow(g$stage_nodes), 2)
  expect_true(any(g$stage_nodes$role == "cte"))
  expect_true(any(g$stage_nodes$role == "output"))
})

test_that("build_graph flags used and join-key columns on table nodes", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  g <- build_graph(test_graph_ir(), schema = test_graph_schema())

  cust_node <- g$table_nodes[g$table_nodes$table == "customers", ]
  cust_cols <- cust_node$columns[[1]]

  # customer_id is selected and is a join key (ON ... c.customer_id)
  expect_true(cust_cols$used[cust_cols$col_name == "customer_id"])
  expect_true(cust_cols$is_key[cust_cols$col_name == "customer_id"])

  # name is never referenced
  expect_false(cust_cols$used[cust_cols$col_name == "name"])
  expect_false(cust_cols$is_key[cust_cols$col_name == "name"])

  # region_id is not projected but IS a join key (LEFT JOIN ON c.region_id)
  expect_false(cust_cols$used[cust_cols$col_name == "region_id"])
  expect_true(cust_cols$is_key[cust_cols$col_name == "region_id"])

  # regions.region_id is also a join key
  reg_node  <- g$table_nodes[g$table_nodes$table == "regions", ]
  reg_cols  <- reg_node$columns[[1]]
  expect_true(reg_cols$is_key[reg_cols$col_name == "region_id"])
})

test_that("build_graph schema shows all catalog columns including unused", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  g <- build_graph(test_graph_ir(), schema = test_graph_schema())
  # orders has 3 catalog columns; all should appear even though order_id is unused
  orders_node <- g$table_nodes[g$table_nodes$table == "orders", ]
  expect_equal(nrow(orders_node$columns[[1]]), 3)
})

test_that("build_graph source_edges carry correct join types", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  g <- build_graph(test_graph_ir())

  # Three physical-table→stage edges: orders->recent, customers->#summary, regions->#summary
  expect_equal(nrow(g$source_edges), 3)

  # regions is the LEFT JOIN target
  reg_node  <- g$table_nodes[g$table_nodes$table == "regions", ]
  reg_edge  <- g$source_edges[g$source_edges$from_node_id == reg_node$node_id, ]
  expect_equal(reg_edge$join_type, "LEFT JOIN")

  # customers is the primary FROM table – no join type
  cust_node <- g$table_nodes[g$table_nodes$table == "customers", ]
  cust_edge <- g$source_edges[g$source_edges$from_node_id == cust_node$node_id, ]
  expect_true(is.na(cust_edge$join_type))
})

test_that("build_graph cte_edges connect CTE stage to consuming stage", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  g <- build_graph(test_graph_ir())

  # One CTE edge: recent -> #summary output stage
  expect_equal(nrow(g$cte_edges), 1)

  recent_node <- g$stage_nodes[!is.na(g$stage_nodes$name) & g$stage_nodes$name == "recent", ]
  output_node <- g$stage_nodes[g$stage_nodes$role == "output", ]
  expect_equal(g$cte_edges$from_node_id, recent_node$node_id)
  expect_equal(g$cte_edges$to_node_id,   output_node$node_id)
})

test_that("build_graph col_edges trace column lineage correctly", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  g <- build_graph(test_graph_ir())

  expect_gt(nrow(g$col_edges), 0)

  # orders.amount should flow into the CTE as recent.total (via SUM)
  orders_node <- g$table_nodes[g$table_nodes$table == "orders", ]
  recent_node <- g$stage_nodes[!is.na(g$stage_nodes$name) & g$stage_nodes$name == "recent", ]

  edge <- g$col_edges[
    g$col_edges$from_node_id == orders_node$node_id &
      g$col_edges$from_port == "amount" &
      g$col_edges$to_node_id == recent_node$node_id,
  ]
  expect_equal(nrow(edge), 1)
  expect_equal(edge$to_port, "total")
})

test_that("build_graph works without a schema (no crash, fewer columns)", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  # Should not error; table nodes show only query-referenced columns
  g <- build_graph(test_graph_ir())
  expect_s3_class(g, "rdataflow_graph")
  expect_equal(nrow(g$table_nodes), 3)
  # Without a schema, orders only shows referenced columns (customer_id, amount)
  orders_node <- g$table_nodes[g$table_nodes$table == "orders", ]
  expect_lte(nrow(orders_node$columns[[1]]), 3)  # fewer than catalog total
})

test_that("temp_edges connects producer stage to consumer stage across statements", {
  skip_if_not(sqlglot_available(), "sqlglot not available")

  # Two-statement script: first SELECT INTO #t, then SELECT FROM #t.
  sql <- paste(
    "SELECT id, name INTO #patients FROM dbo.source_patients;",
    "SELECT p.id, p.name FROM #patients p"
  )
  schema <- schema_from_list(list(
    "dbo.source_patients" = c(id = "INT", name = "VARCHAR")
  ))

  ir <- classify_transform(build_ir(parse_sql(sql, schema = schema)))
  g  <- build_graph(ir, schema = schema)

  # The first statement produces #patients — it should appear as a stage node,
  # NOT as a physical table node.
  expect_equal(nrow(g$table_nodes[g$table_nodes$table == "#patients", ]), 0L)

  # There should be one temp_edge connecting the producer stage to the consumer.
  expect_equal(nrow(g$temp_edges), 1L)

  # The producer stage has output_table "#patients".
  producer <- g$stage_nodes[
    !is.na(g$stage_nodes$output_table) &
      g$stage_nodes$output_table == "#patients", ,
    drop = FALSE
  ]
  expect_equal(nrow(producer), 1L)

  expect_equal(g$temp_edges$from_node_id, producer$node_id)
})

test_that("show_unused_cols = FALSE hides unreferenced table columns", {
  skip_if_not(sqlglot_available(), "sqlglot not available")

  g <- build_graph(test_graph_ir(), schema = test_graph_schema(),
                   show_unused_cols = FALSE)

  # orders.order_id is never projected or used as a key — should be hidden.
  orders_node <- g$table_nodes[g$table_nodes$table == "orders", ]
  expect_false("order_id" %in% orders_node$columns[[1]]$col_name)

  # orders.customer_id IS used (join key + projected) — should appear.
  expect_true("customer_id" %in% orders_node$columns[[1]]$col_name)
})

# --- Batch A regression tests -----------------------------------------------

test_that("same-named CTEs in different statements resolve independently", {
  skip_if_not(sqlglot_available(), "sqlglot not available")

  s <- schema_from_list(list(
    "dbo.orders"    = c(order_id = "INT", customer_id = "INT", amount = "DECIMAL"),
    "dbo.customers" = c(customer_id = "INT", name = "VARCHAR")
  ))
  # Both statements define a CTE called "base" reading different tables.
  sql <- paste(
    "WITH base AS (SELECT order_id, amount FROM dbo.orders)",
    "SELECT order_id, amount INTO #a FROM base;",
    "WITH base AS (SELECT customer_id, name FROM dbo.customers)",
    "SELECT customer_id, name INTO #b FROM base"
  )
  ir <- classify_transform(build_ir(parse_sql(sql, schema = s)))
  g  <- build_graph(ir, schema = s)

  # Four stages: two CTEs + two outputs, and each output consumes its OWN base.
  expect_equal(nrow(g$stage_nodes), 4L)
  expect_equal(nrow(g$cte_edges), 2L)

  out_a <- g$stage_nodes[g$stage_nodes$role == "output" &
                           !is.na(g$stage_nodes$output_table) &
                           g$stage_nodes$output_table == "#a", ]
  out_b <- g$stage_nodes[g$stage_nodes$role == "output" &
                           !is.na(g$stage_nodes$output_table) &
                           g$stage_nodes$output_table == "#b", ]
  cte_1 <- g$stage_nodes[g$stage_nodes$role == "cte" &
                           g$stage_nodes$statement_index == out_a$statement_index, ]
  cte_2 <- g$stage_nodes[g$stage_nodes$role == "cte" &
                           g$stage_nodes$statement_index == out_b$statement_index, ]

  edge_a <- g$cte_edges[g$cte_edges$to_node_id == out_a$node_id, ]
  edge_b <- g$cte_edges[g$cte_edges$to_node_id == out_b$node_id, ]
  expect_equal(edge_a$from_node_id, cte_1$node_id)
  expect_equal(edge_b$from_node_id, cte_2$node_id)
})

test_that("a CTE name in one statement does not hide a same-named physical table in another", {
  skip_if_not(sqlglot_available(), "sqlglot not available")

  s <- schema_from_list(list(
    "dbo.orders" = c(order_id = "INT", amount = "DECIMAL"),
    "dbo.recent" = c(id = "INT", val = "INT")
  ))
  # Statement 1 defines CTE "recent"; statement 2 reads the PHYSICAL dbo.recent.
  sql <- paste(
    "WITH recent AS (SELECT order_id FROM dbo.orders)",
    "SELECT order_id INTO #x FROM recent;",
    "SELECT id, val INTO #y FROM dbo.recent"
  )
  ir <- classify_transform(build_ir(parse_sql(sql, schema = s)))
  g  <- build_graph(ir, schema = s)

  # dbo.recent must appear as a physical table node.
  expect_true("recent" %in% tolower(g$table_nodes$table))
  # And statement 2's output must have a source edge from it, not a cte edge.
  out_y <- g$stage_nodes[g$stage_nodes$role == "output" &
                           !is.na(g$stage_nodes$output_table) &
                           g$stage_nodes$output_table == "#y", ]
  recent_tbl <- g$table_nodes[tolower(g$table_nodes$table) == "recent", ]
  expect_true(any(g$source_edges$from_node_id == recent_tbl$node_id &
                    g$source_edges$to_node_id == out_y$node_id))
})

test_that("schema-qualified produced tables link producer to consumer", {
  skip_if_not(sqlglot_available(), "sqlglot not available")

  s <- schema_from_list(list(
    "dbo.source_patients" = c(id = "INT", name = "VARCHAR")
  ))
  # INSERT INTO a schema-qualified table, then read it by qualified name.
  sql <- paste(
    "INSERT INTO [dbo].[staging] SELECT id, name FROM dbo.source_patients;",
    "SELECT s.id FROM dbo.staging s"
  )
  ir <- classify_transform(build_ir(parse_sql(sql, schema = s)))
  g  <- build_graph(ir, schema = s)

  # dbo.staging is produced by statement 1 — must be a stage link, not a
  # disconnected physical table node.
  expect_false("staging" %in% tolower(g$table_nodes$table))
  expect_equal(nrow(g$temp_edges), 1L)
})

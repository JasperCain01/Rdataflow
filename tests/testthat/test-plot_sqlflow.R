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
  # Suppress legend so the colour check is not confused by legend content.
  dot <- graph_to_dot(g, show_col_edges = FALSE, show_legend = FALSE)
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

test_that("graph_to_dot includes legend by default", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  g   <- test_plot_ir()
  dot <- graph_to_dot(g)
  expect_match(dot, "cluster_legend")
  expect_match(dot, "legend_node")
  # All key legend sections should be present
  expect_match(dot, "Node headers")
  expect_match(dot, "Column role")
  expect_match(dot, "Transformation")
  expect_match(dot, "Edges")
})

test_that("graph_to_dot omits legend when show_legend = FALSE", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  g   <- test_plot_ir()
  dot <- graph_to_dot(g, show_legend = FALSE)
  expect_false(grepl("cluster_legend", dot, fixed = TRUE))
})

test_that("show_legend parameter passes through sql_dataflow to plot", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  g   <- test_plot_ir()
  # graph_to_dot with show_legend = FALSE produces no legend
  dot_no  <- graph_to_dot(g, show_legend = FALSE)
  dot_yes <- graph_to_dot(g, show_legend = TRUE)
  expect_false(grepl("Legend", dot_no, fixed = TRUE))
  expect_true(grepl("Legend", dot_yes, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# Rank lane tests
# ---------------------------------------------------------------------------

test_that("compute_node_ranks assigns depth 0 to table nodes", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  g     <- test_plot_ir()
  ranks <- compute_node_ranks(g)
  tbl_ranks <- ranks[g$table_nodes$node_id]
  expect_true(all(tbl_ranks == 0L))
})

test_that("compute_node_ranks assigns depth >= 1 to all stage nodes", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  g     <- test_plot_ir()
  ranks <- compute_node_ranks(g)
  stg_ranks <- ranks[g$stage_nodes$node_id]
  expect_true(all(!is.na(stg_ranks)))
  expect_true(all(stg_ranks >= 1L))
})

test_that("dot_rank_constraints produces one block per depth level", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  g      <- test_plot_ir()
  ranks  <- compute_node_ranks(g)
  blocks <- dot_rank_constraints(ranks)
  # Every block must start with the rank=same pattern
  expect_true(all(grepl("rank=same", blocks, fixed = TRUE)))
  # Physical-table nodes (depth 0) must NOT appear in any rank block
  for (nid in g$table_nodes$node_id) {
    expect_false(any(grepl(nid, blocks, fixed = TRUE)))
  }
})

test_that("graph_to_dot includes rank=same by default", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  g   <- test_plot_ir()
  dot <- graph_to_dot(g)
  expect_match(dot, "rank=same")
})

test_that("graph_to_dot omits rank=same when rank_lanes = FALSE", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  g   <- test_plot_ir()
  dot <- graph_to_dot(g, rank_lanes = FALSE)
  expect_false(grepl("rank=same", dot, fixed = TRUE))
})

# --- Batch C regression tests -----------------------------------------------

test_that("stage columns carry expression tooltips in the DOT output", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  ir <- classify_transform(build_ir(parse_sql(
    "SELECT SUM(amount) AS total FROM dbo.orders GROUP BY customer_id"
  )))
  g <- build_graph(ir)
  dot <- graph_to_dot(g)
  expect_match(dot, 'TOOLTIP="SUM', fixed = TRUE)
})

test_that("WHERE predicates render as stage footers", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  ir <- classify_transform(build_ir(parse_sql(
    "SELECT a FROM dbo.t WHERE b > 5"
  )))
  g <- build_graph(ir)
  dot <- graph_to_dot(g)
  expect_match(dot, "WHERE", fixed = TRUE)
})

test_that("join keys appear on structural edges", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  ir <- classify_transform(build_ir(parse_sql(paste(
    "SELECT c.customer_id, o.amount FROM dbo.customers c",
    "LEFT JOIN dbo.orders o ON o.customer_id = c.customer_id"
  ))))
  g <- build_graph(ir)
  dot <- graph_to_dot(g, show_col_edges = FALSE)
  expect_match(dot, "LEFT JOIN\\n", fixed = TRUE)   # label contains keys
  expect_match(dot, "customer_id = ", fixed = TRUE)
})

test_that("column-edge mode still draws faint structural join edges", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  ir <- classify_transform(build_ir(parse_sql(paste(
    "SELECT c.customer_id FROM dbo.customers c",
    "LEFT JOIN dbo.orders o ON o.customer_id = c.customer_id"
  ))))
  g <- build_graph(ir)
  dot <- graph_to_dot(g, show_col_edges = TRUE)
  expect_match(dot, "LEFT JOIN", fixed = TRUE)
})

test_that("legend is dynamic — absent categories are not listed", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  # A single passthrough select: no window/date/case/cast anywhere.
  ir <- classify_transform(build_ir(parse_sql("SELECT a FROM dbo.t")))
  g <- build_graph(ir)
  dot <- graph_to_dot(g, show_legend = TRUE)
  expect_false(grepl(">window<", dot))
  expect_false(grepl(">cast<", dot))
  expect_true(grepl("Legend", dot))
})

test_that("max_cols truncates wide tables with an overflow row", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  s <- schema_from_list(list(
    "dbo.wide" = stats::setNames(rep("INT", 20), paste0("c", 1:20))
  ))
  ir <- classify_transform(build_ir(parse_sql(
    "SELECT c1 FROM dbo.wide", schema = s
  )))
  g <- build_graph(ir, schema = s, max_cols = 5)
  wide <- g$table_nodes[g$table_nodes$table == "wide", ]
  expect_equal(nrow(wide$columns[[1]]), 5L)
  expect_equal(wide$n_hidden, 15L)
  expect_match(graph_to_dot(g), "more columns")
  # projected column is always kept
  expect_true("c1" %in% wide$columns[[1]]$col_name)
})

test_that("rankdir=TB is honoured and save_sqlflow writes DOT files", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  ir <- classify_transform(build_ir(parse_sql("SELECT a FROM dbo.t")))
  g <- build_graph(ir)
  expect_match(graph_to_dot(g, rankdir = "TB"), "rankdir=TB", fixed = TRUE)

  path <- tempfile(fileext = ".dot")
  save_sqlflow(g, path)
  expect_true(file.exists(path))
  expect_match(paste(readLines(path), collapse = "\n"), "digraph sqlflow")
  unlink(path)
})

# --- Batch H regression tests (indirect lineage: condition/partition/filter) -

test_that("condition/partition column edges render dotted with role in the tooltip", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  s <- schema_from_list(list(
    "dbo.t" = c(status = "INT", amount = "DECIMAL", x = "INT",
               grp = "INT", d = "DATE")
  ))
  sql <- paste(
    "SELECT CASE WHEN status = 1 THEN amount ELSE 0 END AS adj,",
    "SUM(x) OVER (PARTITION BY grp ORDER BY d) AS running FROM dbo.t"
  )
  ir <- classify_transform(build_ir(parse_sql(sql, schema = s)))
  dot <- graph_to_dot(build_graph(ir, schema = s))

  expect_match(dot, 'tbl_dbo_t:status -> stg_1_result:adj \\[style=dotted color="#9bb8d4"')
  expect_match(dot, "tooltip=\"status -> adj \\(condition\\)\"")
  expect_match(dot, "tooltip=\"grp -> running \\(partition\\)\"")
  # THEN/ELSE and window-argument columns keep the plain value styling
  expect_match(dot, 'tbl_dbo_t:amount -> stg_1_result:adj \\[style=dashed color="#4a90d9"')
})

test_that("filter column edges render dotted grey and target the stage node (no port)", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  s <- schema_from_list(list("dbo.t" = c(a = "INT", b = "INT")))
  ir <- classify_transform(build_ir(parse_sql("SELECT a FROM dbo.t WHERE b > 5", schema = s)))
  g <- build_graph(ir, schema = s)
  dot <- graph_to_dot(g)

  stg_node <- g$stage_nodes[g$stage_nodes$role == "output", ]$node_id
  expect_match(dot, sprintf('tbl_dbo_t:b -> %s \\[style=dotted color="#aaaaaa"', stg_node))
  expect_match(dot, 'tooltip="b \\(filter\\)"')
})

test_that("legend lists condition/partition and filter entries only when present", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  # Plain value-only lineage: no condition/partition/filter legend rows.
  plain_dot <- graph_to_dot(test_plot_ir())
  expect_false(grepl("Condition/partition column", plain_dot, fixed = TRUE))
  expect_false(grepl("Filter column", plain_dot, fixed = TRUE))

  s <- schema_from_list(list("dbo.t" = c(a = "INT", b = "INT")))
  ir <- classify_transform(build_ir(parse_sql("SELECT a FROM dbo.t WHERE b > 5", schema = s)))
  filter_dot <- graph_to_dot(build_graph(ir, schema = s))
  expect_match(filter_dot, "Filter column (WHERE)", fixed = TRUE)
  expect_false(grepl("Condition/partition column", filter_dot, fixed = TRUE))
})

# --- Batch J regression tests (HAVING / DISTINCT / TOP) ---------------------

test_that("html_stage_label renders a combined DISTINCT/TOP/HAVING footer", {
  html <- html_stage_label(
    display_name = "result", role = "output",
    columns_tbl = tibble::tibble(col_name = "a", expr = NA_character_,
                                 transform_type = "passthrough"),
    transform_label = "",
    distinct = TRUE, top = "100", having = "SUM(amount) > 100"
  )
  expect_match(html, "DISTINCT; TOP 100; HAVING SUM\\(amount\\) &gt; 100")
})

test_that("html_stage_label omits the modifier footer when none apply", {
  html <- html_stage_label(
    display_name = "result", role = "output",
    columns_tbl = tibble::tibble(col_name = "a", expr = NA_character_,
                                 transform_type = "passthrough"),
    transform_label = ""
  )
  expect_false(grepl("DISTINCT", html, fixed = TRUE))
  expect_false(grepl("TOP", html, fixed = TRUE))
  expect_false(grepl("HAVING", html, fixed = TRUE))
})

test_that("graph_to_dot renders DISTINCT/TOP/HAVING for a real query", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  s <- schema_from_list(list("dbo.orders" = c(customer_id = "INT", amount = "DECIMAL")))
  sql <- paste(
    "SELECT DISTINCT TOP 100 customer_id, SUM(amount) AS total",
    "FROM dbo.orders GROUP BY customer_id HAVING SUM(amount) > 100"
  )
  ir <- classify_transform(build_ir(parse_sql(sql, schema = s)))
  dot <- graph_to_dot(build_graph(ir, schema = s))
  expect_match(dot, "DISTINCT; TOP 100; HAVING SUM")
})

test_that("long HAVING predicates truncate in the footer with the full text on hover", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  s <- schema_from_list(list("dbo.orders" = c(customer_id = "INT", amount = "DECIMAL")))
  long_having <- paste(sprintf("amount > %d", 1:10), collapse = " OR ")
  sql <- sprintf(
    "SELECT customer_id, SUM(amount) AS total FROM dbo.orders GROUP BY customer_id HAVING %s",
    long_having
  )
  ir <- classify_transform(build_ir(parse_sql(sql, schema = s)))
  dot <- graph_to_dot(build_graph(ir, schema = s))
  expect_match(dot, "HAVING .*amount.* &gt; 1.*\\.\\.\\.")
  expect_match(dot, "TOOLTIP=\"HAVING .*amount.* &gt; 1")
})

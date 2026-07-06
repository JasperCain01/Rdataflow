# Regenerate the example diagrams embedded in the vignette and README.
#
# DiagrammeR / DiagrammeRsvg could not be installed in the sandbox this was
# authored in (CRAN was unreachable behind the outbound network policy), so
# instead of routing through DiagrammeR this script writes the raw Graphviz
# DOT via save_sqlflow(*.dot) and shells out to the system `dot` binary
# (apt package `graphviz`) to rasterise real SVGs from it. Re-run this after
# any change to plot_sqlflow.R that affects rendering, or on a machine with
# DiagrammeR installed if you'd rather call save_sqlflow(*.svg) directly.
#
# Usage: Rscript data-raw/make_figures.R   (run from the package root)

pkgload::load_all(".")

fig_dir <- "man/figures"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

render <- function(graph, name, ...) {
  dot_path <- file.path(fig_dir, paste0(name, ".dot"))
  svg_path <- file.path(fig_dir, paste0(name, ".svg"))
  save_sqlflow(graph, dot_path, ...)
  status <- tryCatch(
    system2("dot", c("-Tsvg", shQuote(dot_path), "-o", shQuote(svg_path))),
    error = function(e) 1L
  )
  if (!identical(status, 0L)) {
    warning(sprintf("Could not rasterise %s with `dot` (graphviz not installed?)", name))
  }
  invisible(svg_path)
}

s <- schema_from_list(list(
  "dbo.customers" = c(customer_id = "INT", name = "VARCHAR", region_id = "INT"),
  "dbo.orders"    = c(order_id = "INT", customer_id = "INT", amount = "DECIMAL"),
  "dbo.regions"   = c(region_id = "INT", region_name = "VARCHAR")
))

quickstart_sql <- "
  WITH recent AS (
    SELECT o.customer_id, SUM(o.amount) AS total, COUNT(*) AS n
    FROM dbo.orders o
    GROUP BY o.customer_id
  )
  SELECT
    c.customer_id,
    r.region_name,
    recent.total,
    recent.n
  INTO #summary
  FROM dbo.customers c
  JOIN   recent          ON recent.customer_id = c.customer_id
  LEFT JOIN dbo.regions r ON r.region_id      = c.region_id
"

g <- build_graph(
  classify_transform(build_ir(parse_sql(quickstart_sql, schema = s))),
  schema = s
)
render(g, "fig-quickstart")

# Structural overview: table -> stage edges only, no column-level lineage.
render(g, "fig-overview", show_col_edges = FALSE)

# max_cols demo: a wide table with only a few columns actually referenced.
wide_schema <- schema_from_list(list(
  "dbo.customers" = c(
    customer_id = "INT", name = "VARCHAR", region_id = "INT",
    email = "VARCHAR", phone = "VARCHAR", address1 = "VARCHAR",
    address2 = "VARCHAR", city = "VARCHAR", state = "VARCHAR",
    postal_code = "VARCHAR", country = "VARCHAR", created_at = "DATETIME",
    updated_at = "DATETIME", loyalty_tier = "VARCHAR", notes = "VARCHAR"
  )
))
maxcols_sql <- "
  SELECT customer_id, name, region_id
  FROM dbo.customers
"
g_wide <- build_graph(
  classify_transform(build_ir(parse_sql(maxcols_sql, schema = wide_schema))),
  schema = wide_schema, max_cols = 6
)
render(g_wide, "fig-maxcols")

# Multi-statement script: temp-table chaining across statements, each
# statement's stages drawn inside its own labelled cluster.
multi_sql <- "
  SELECT o.customer_id, SUM(o.amount) AS total
  INTO #totals
  FROM dbo.orders o
  GROUP BY o.customer_id;

  SELECT c.customer_id, c.name, t.total
  INTO #summary
  FROM dbo.customers c
  JOIN #totals t ON t.customer_id = c.customer_id;
"
g_multi <- build_graph(
  classify_transform(build_ir(parse_sql(multi_sql, schema = s))),
  schema = s
)
render(g_multi, "fig-multistatement")

cat("Figures written to", fig_dir, "\n")

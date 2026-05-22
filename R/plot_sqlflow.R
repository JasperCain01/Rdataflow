# ---------------------------------------------------------------------------
# Renderer
#
# plot_sqlflow() turns an rdataflow_graph into a Graphviz diagram rendered
# through DiagrammeR. HTML-like table labels give each node a header plus
# one row per column; colours signal column role (table nodes) or
# transformation type (stage nodes).
#
# Two rendering modes (controlled by show_col_edges):
#   TRUE  (default) - column-level port-to-port edges show exact lineage
#   FALSE           - structural table→stage / CTE→stage edges only, labelled
#                     with join type; useful for high-level overview
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Package-level colour constants (internal, not exported)
# ---------------------------------------------------------------------------

# Node header backgrounds
.tbl_header_bg <- "#34495e"   # dark slate  – physical table headers
.cte_header_bg <- "#1e6b45"   # dark green  – CTE stage headers
.out_header_bg <- "#1a4f7a"   # dark blue   – output stage headers

# Table column backgrounds
.col_used_bg   <- "#d4edff"   # light blue  – column is projected
.col_key_bg    <- "#fff3cd"   # light amber – column is a join key only
.col_both_bg   <- "#c3d9f5"   # mid blue    – projected AND a key
.col_none_bg   <- "#f9f9f9"   # near-white  – unreferenced column

# Stage column backgrounds by transformation type
.transform_colors <- c(
  aggregate   = "#fff3cd",
  window      = "#d1ecf1",
  date        = "#cfe2ff",
  `case`      = "#e2d9f3",
  cast        = "#fde8d8",
  string      = "#fce4ec",
  arithmetic  = "#fef9c4",
  expression  = "#e9ecef",
  passthrough = "#ffffff"
)

.transform_label_bg <- "#f0f0f0"  # footer row for stage transform summary

# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

#' Render a SQL dataflow graph
#'
#' @param graph An `rdataflow_graph` produced by [build_graph()].
#' @param show_col_edges If `TRUE` (default), draw column-level port-to-port
#'   edges showing exact column lineage. If `FALSE`, draw structural
#'   table→stage edges only, labelled with join types.
#' @param show_legend If `TRUE` (default), append a colour-coding legend
#'   cluster to the diagram explaining node header colours, column role
#'   colours, transformation type colours, and edge styles.
#'
#' @return A `DiagrammeR` htmlwidget for display in RStudio, R Markdown, or
#'   Shiny.
#' @export
plot_sqlflow <- function(graph, show_col_edges = TRUE, show_legend = TRUE) {
  if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
    rlang::abort(paste(
      "Package 'DiagrammeR' is required.",
      "Install it with install.packages('DiagrammeR')."
    ))
  }
  stopifnot(inherits(graph, "rdataflow_graph"))
  DiagrammeR::grViz(
    graph_to_dot(graph, show_col_edges = show_col_edges, show_legend = show_legend)
  )
}

#' Export the Graphviz DOT source for a dataflow graph
#'
#' Returns the raw DOT string that [plot_sqlflow()] sends to Graphviz.
#' Useful for inspecting the generated markup, saving to a file, or
#' customising the diagram manually.
#'
#' @inheritParams plot_sqlflow
#' @return A length-1 character string of valid DOT code.
#' @export
graph_to_dot <- function(graph, show_col_edges = TRUE, show_legend = TRUE) {
  stopifnot(inherits(graph, "rdataflow_graph"))

  # Generate one DOT node statement per table node and stage node.
  tbl_stmts <- purrr::map_chr(
    seq_len(nrow(graph$table_nodes)),
    function(i) dot_table_node(graph$table_nodes[i, ])
  )
  stg_stmts <- purrr::map_chr(
    seq_len(nrow(graph$stage_nodes)),
    function(i) dot_stage_node(graph$stage_nodes[i, ])
  )

  # Edge statements differ by mode.
  edge_stmts <- if (show_col_edges) {
    build_col_edge_stmts(graph$col_edges)
  } else {
    c(
      build_source_edge_stmts(graph$source_edges),
      build_cte_edge_stmts(graph$cte_edges),
      build_temp_edge_stmts(graph$temp_edges)
    )
  }

  # Optional legend cluster appended before the closing brace.
  legend_stmts <- if (isTRUE(show_legend)) dot_legend_subgraph() else character(0)

  paste(
    c(dot_preamble(), tbl_stmts, "", stg_stmts, "", edge_stmts, legend_stmts, "}"),
    collapse = "\n"
  )
}

# ---------------------------------------------------------------------------
# DOT preamble
# ---------------------------------------------------------------------------

dot_preamble <- function() {
  c(
    "digraph sqlflow {",
    '  graph [rankdir=LR bgcolor="#fafafa" fontname="Helvetica" pad="0.4"',
    '         nodesep="0.4" ranksep="0.9" splines=polyline]',
    '  node  [shape=none margin="0" fontname="Helvetica"]',
    '  edge  [fontname="Helvetica" fontsize="9" color="#888888"]',
    ""
  )
}

# ---------------------------------------------------------------------------
# Node statement builders
# ---------------------------------------------------------------------------

# Produce a single DOT node statement for a physical table node row.
dot_table_node <- function(row) {
  html  <- html_table_label(row$label, row$columns[[1]])
  sprintf('  %s [label=<%s>]', row$node_id, html)
}

# Produce a single DOT node statement for a stage (CTE / output) node row.
dot_stage_node <- function(row) {
  html <- html_stage_label(
    display_name    = row$display_name,
    role            = row$role,
    columns_tbl     = row$columns[[1]],
    transform_label = row$transform_label
  )
  sprintf('  %s [label=<%s>]', row$node_id, html)
}

# ---------------------------------------------------------------------------
# HTML label builders
# ---------------------------------------------------------------------------

# Build the HTML table label for a physical source table node.
# columns_tbl has cols: col_name, col_type, used, is_key.
html_table_label <- function(table_label, columns_tbl) {
  # Header row: dark background, white bold table name.
  header <- sprintf(
    paste0(
      '<TR><TD COLSPAN="2" BGCOLOR="%s" ALIGN="LEFT">',
      '<FONT COLOR="white"><B>%s</B></FONT></TD></TR>'
    ),
    .tbl_header_bg, html_esc(table_label)
  )

  # One row per column with colour-coded background.
  col_rows <- purrr::map_chr(seq_len(nrow(columns_tbl)), function(i) {
    col   <- columns_tbl[i, ]
    bg    <- col_bgcolor(col$used, col$is_key)
    pname <- port_id(col$col_name)

    # Bold the column name when it is a join key (visual emphasis).
    name_html <- if (isTRUE(col$is_key)) {
      sprintf("<B>%s</B>", html_esc(col$col_name))
    } else {
      html_esc(col$col_name)
    }

    type_html <- if (!is.na(col$col_type)) {
      sprintf('<FONT COLOR="#999999">%s</FONT>', html_esc(col$col_type))
    } else {
      ""
    }

    sprintf(
      paste0(
        '<TR>',
        '<TD PORT="%s" BGCOLOR="%s" ALIGN="LEFT">%s</TD>',
        '<TD BGCOLOR="%s" ALIGN="LEFT">%s</TD>',
        '</TR>'
      ),
      pname, bg, name_html, bg, type_html
    )
  })

  rows_str <- paste(c(header, col_rows), collapse = "")
  sprintf(
    '<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="3">%s</TABLE>',
    rows_str
  )
}

# Build the HTML table label for a stage node.
# columns_tbl has cols: col_name, transform_type.
html_stage_label <- function(display_name, role, columns_tbl, transform_label) {
  header_bg  <- if (identical(role, "cte")) .cte_header_bg else .out_header_bg
  role_label <- if (identical(role, "cte")) "CTE" else "output"

  header <- sprintf(
    paste0(
      '<TR><TD COLSPAN="2" BGCOLOR="%s" ALIGN="LEFT">',
      '<FONT COLOR="white"><B>%s</B>',
      ' <FONT POINT-SIZE="9">(%s)</FONT>',
      '</FONT></TD></TR>'
    ),
    header_bg, html_esc(display_name), role_label
  )

  col_rows <- purrr::map_chr(seq_len(nrow(columns_tbl)), function(i) {
    col   <- columns_tbl[i, ]
    tt    <- if (!is.na(col$transform_type)) col$transform_type else "passthrough"
    bg    <- transform_bgcolor(tt)
    pname <- port_id(col$col_name)

    # Show transform type abbreviation only for non-passthrough columns.
    type_html <- if (!identical(tt, "passthrough")) {
      sprintf('<FONT COLOR="#777777">%s</FONT>', html_esc(tt))
    } else {
      ""
    }

    sprintf(
      paste0(
        '<TR>',
        '<TD PORT="%s" BGCOLOR="%s" ALIGN="LEFT">%s</TD>',
        '<TD BGCOLOR="%s" ALIGN="LEFT">%s</TD>',
        '</TR>'
      ),
      pname, bg, html_esc(col$col_name), bg, type_html
    )
  })

  # Footer row: italic transform summary (only when the label is non-empty).
  footer <- if (!is.null(transform_label) && nzchar(transform_label)) {
    sprintf(
      paste0(
        '<TR><TD COLSPAN="2" BGCOLOR="%s" ALIGN="LEFT">',
        '<FONT COLOR="#555555" POINT-SIZE="9"><I>%s</I></FONT>',
        '</TD></TR>'
      ),
      .transform_label_bg, html_esc(transform_label)
    )
  } else {
    NULL
  }

  rows_str <- paste(c(header, col_rows, footer), collapse = "")
  sprintf(
    '<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="3">%s</TABLE>',
    rows_str
  )
}

# ---------------------------------------------------------------------------
# Edge statement builders
# ---------------------------------------------------------------------------

# Build DOT edge statements for column-level lineage (port-to-port edges).
build_col_edge_stmts <- function(col_edges) {
  if (nrow(col_edges) == 0) return(character(0))
  purrr::map_chr(seq_len(nrow(col_edges)), function(i) {
    row <- col_edges[i, ]
    fp  <- port_id(row$from_port)
    tp  <- port_id(row$to_port)
    sprintf(
      '  %s:%s -> %s:%s [style=dashed color="#4a90d9" arrowsize=0.7]',
      row$from_node_id, fp, row$to_node_id, tp
    )
  })
}

# Build DOT edge statements for structural table→stage connections.
# join_type is NA for the primary FROM table and a string like "LEFT JOIN"
# for joined tables.
build_source_edge_stmts <- function(source_edges) {
  if (nrow(source_edges) == 0) return(character(0))
  purrr::map_chr(seq_len(nrow(source_edges)), function(i) {
    row <- source_edges[i, ]

    if (!is.na(row$join_type)) {
      # Labelled with the join type; use orange to distinguish joins.
      sprintf(
        '  %s -> %s [label="%s" color="#cc6600" fontcolor="#cc6600"]',
        row$from_node_id, row$to_node_id,
        html_esc(row$join_type)
      )
    } else {
      sprintf('  %s -> %s', row$from_node_id, row$to_node_id)
    }
  })
}

# Build DOT edge statements for CTE→downstream-stage connections.
build_cte_edge_stmts <- function(cte_edges) {
  if (nrow(cte_edges) == 0) return(character(0))
  purrr::map_chr(seq_len(nrow(cte_edges)), function(i) {
    row <- cte_edges[i, ]
    sprintf(
      '  %s -> %s [style=dashed color="#666666" label="CTE" fontcolor="#666666"]',
      row$from_node_id, row$to_node_id
    )
  })
}

# Build DOT edge statements for temp-table producer→consumer stage connections.
# Uses a distinct style from CTE edges so the two are visually distinguishable.
build_temp_edge_stmts <- function(temp_edges) {
  if (is.null(temp_edges) || nrow(temp_edges) == 0) return(character(0))
  purrr::map_chr(seq_len(nrow(temp_edges)), function(i) {
    row <- temp_edges[i, ]
    sprintf(
      '  %s -> %s [style=dashed color="#4477AA" label="#temp" fontcolor="#4477AA"]',
      row$from_node_id, row$to_node_id
    )
  })
}

# ---------------------------------------------------------------------------
# Legend subgraph
# ---------------------------------------------------------------------------

# Build a Graphviz cluster subgraph containing a colour-coding legend.
# Returns a character vector of DOT lines ready to splice into the graph body.
dot_legend_subgraph <- function() {
  # Section divider row inside the legend table.
  legend_section <- function(label) {
    sprintf(
      paste0(
        '<TR><TD BGCOLOR="#e0e0e0" ALIGN="LEFT">',
        '<FONT POINT-SIZE="9"><B>%s</B></FONT>',
        '</TD></TR>'
      ),
      label
    )
  }

  # Colored swatch row — background is the visual indicator; font_color lets
  # white text appear on dark swatches.
  legend_swatch <- function(label, bg, font_color = "#222222") {
    sprintf(
      paste0(
        '<TR><TD BGCOLOR="%s" ALIGN="LEFT">',
        '<FONT POINT-SIZE="9" COLOR="%s">%s</FONT>',
        '</TD></TR>'
      ),
      bg, font_color, label
    )
  }

  # Edge-type row — colored text with ASCII dashes to suggest line style.
  legend_edge <- function(label, color, dashed = FALSE) {
    prefix <- if (dashed) "- -  " else "---  "
    sprintf(
      paste0(
        '<TR><TD ALIGN="LEFT">',
        '<FONT POINT-SIZE="9" COLOR="%s">%s%s</FONT>',
        '</TD></TR>'
      ),
      color, prefix, label
    )
  }

  rows <- c(
    # Title
    paste0(
      '<TR><TD BGCOLOR="#555555" ALIGN="LEFT">',
      '<FONT COLOR="white" POINT-SIZE="10"><B>Legend</B></FONT>',
      '</TD></TR>'
    ),

    # Node header colours
    legend_section("Node headers"),
    legend_swatch("Physical table", .tbl_header_bg, "white"),
    legend_swatch("CTE stage",      .cte_header_bg, "white"),
    legend_swatch("Output stage",   .out_header_bg, "white"),

    # Column role colours (table nodes)
    legend_section("Column role (table nodes)"),
    legend_swatch("Projected",       .col_used_bg),
    legend_swatch("Join key only",   .col_key_bg),
    legend_swatch("Projected + key", .col_both_bg),
    legend_swatch("Unused",          .col_none_bg),

    # Transformation type colours (stage nodes)
    legend_section("Transformation (stage nodes)"),
    legend_swatch("aggregate",   .transform_colors[["aggregate"]]),
    legend_swatch("window",      .transform_colors[["window"]]),
    legend_swatch("date",        .transform_colors[["date"]]),
    legend_swatch("case",        .transform_colors[["case"]]),
    legend_swatch("cast",        .transform_colors[["cast"]]),
    legend_swatch("string",      .transform_colors[["string"]]),
    legend_swatch("arithmetic",  .transform_colors[["arithmetic"]]),
    legend_swatch("expression",  .transform_colors[["expression"]]),
    legend_swatch("passthrough", .transform_colors[["passthrough"]]),

    # Edge types
    legend_section("Edges"),
    legend_edge("Source / FROM",  "#888888", dashed = FALSE),
    legend_edge("JOIN",           "#cc6600", dashed = FALSE),
    legend_edge("CTE reference",  "#666666", dashed = TRUE),
    legend_edge("#temp feed",     "#4477AA", dashed = TRUE),
    legend_edge("Column lineage", "#4a90d9", dashed = TRUE)
  )

  table_html <- sprintf(
    '<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="3">%s</TABLE>',
    paste(rows, collapse = "")
  )

  c(
    "",
    "  subgraph cluster_legend {",
    '    graph [style=rounded color="#cccccc" bgcolor="#ffffff" label="" margin="8"]',
    sprintf('    legend_node [label=<%s> shape=none margin="0"]', table_html),
    "  }"
  )
}

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Pick the background colour for a table column based on whether it is
# projected (used) and/or a join key, giving four distinct visual states.
col_bgcolor <- function(used, is_key) {
  used   <- isTRUE(used)
  is_key <- isTRUE(is_key)
  if (used && is_key) .col_both_bg
  else if (used)      .col_used_bg
  else if (is_key)    .col_key_bg
  else                .col_none_bg
}

# Map a transformation type string to its background colour. Falls back to
# the passthrough colour (white) for unrecognised types.
transform_bgcolor <- function(type) {
  hit <- .transform_colors[type]
  if (!is.na(hit)) hit else .transform_colors[["passthrough"]]
}

# Normalise a column name into a valid Graphviz port identifier by replacing
# any character outside [a-zA-Z0-9_] with an underscore.
port_id <- function(col_name) {
  stringr::str_replace_all(as.character(col_name), "[^a-zA-Z0-9_]", "_")
}

# Escape HTML special characters so column names / labels render literally
# inside Graphviz HTML-like labels.
html_esc <- function(x) {
  x <- gsub("&",  "&amp;",  as.character(x), fixed = TRUE)
  x <- gsub("<",  "&lt;",   x, fixed = TRUE)
  x <- gsub(">",  "&gt;",   x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

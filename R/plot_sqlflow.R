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

# Table column backgrounds. Note: the join-key amber is deliberately deeper
# than the aggregate pastel (#fff3cd) used on stage nodes — the two used to
# share a hex, which made the legend claim one colour meant two things.
.col_used_bg   <- "#d4edff"   # light blue   – column is projected
.col_key_bg    <- "#ffd699"   # light orange – column is a join key only
.col_both_bg   <- "#a9c9ee"   # mid blue     – projected AND a key
.col_none_bg   <- "#f9f9f9"   # near-white   – unreferenced column

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
#' @param rank_lanes If `TRUE` (default), insert `rank=same` constraints so
#'   that nodes at the same dependency depth are aligned in the same column.
#'   This turns parallel branches into aligned vertical lanes, making complex
#'   multi-stage scripts easier to follow. Pass `FALSE` to let Graphviz place
#'   nodes freely.
#' @param rankdir Graphviz layout direction: `"LR"` (default, left-to-right)
#'   or `"TB"` (top-to-bottom; useful for tall narrow display areas).
#'
#' @return A `DiagrammeR` htmlwidget for display in RStudio, R Markdown, or
#'   Shiny. Hover a stage column to see the full SQL expression that computes
#'   it; hover a join edge to see the join keys.
#' @export
plot_sqlflow <- function(graph, show_col_edges = TRUE, show_legend = TRUE,
                         rank_lanes = TRUE, rankdir = c("LR", "TB")) {
  if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
    rlang::abort(paste(
      "Package 'DiagrammeR' is required.",
      "Install it with install.packages('DiagrammeR')."
    ))
  }
  stopifnot(inherits(graph, "rdataflow_graph"))
  DiagrammeR::grViz(
    graph_to_dot(
      graph,
      show_col_edges = show_col_edges,
      show_legend    = show_legend,
      rank_lanes     = rank_lanes,
      rankdir        = rankdir
    )
  )
}

#' Save a SQL dataflow diagram to a file
#'
#' Renders the graph and writes it to `file`. The format is chosen by file
#' extension: `.dot` / `.gv` write the raw Graphviz DOT source (no extra
#' dependencies); `.svg`, `.png`, and `.pdf` render via the `DiagrammeRsvg`
#' and `rsvg` packages (install both for image export).
#'
#' @inheritParams plot_sqlflow
#' @param file Output path; extension selects the format
#'   (`.dot`, `.gv`, `.svg`, `.png`, `.pdf`).
#'
#' @return Invisibly, `file`.
#' @export
save_sqlflow <- function(graph, file, show_col_edges = TRUE,
                         show_legend = TRUE, rank_lanes = TRUE,
                         rankdir = c("LR", "TB")) {
  stopifnot(inherits(graph, "rdataflow_graph"), is.character(file),
            length(file) == 1L)
  dot <- graph_to_dot(graph, show_col_edges = show_col_edges,
                      show_legend = show_legend, rank_lanes = rank_lanes,
                      rankdir = rankdir)
  ext <- tolower(tools::file_ext(file))

  if (ext %in% c("dot", "gv")) {
    writeLines(dot, file)
    return(invisible(file))
  }

  if (!ext %in% c("svg", "png", "pdf")) {
    rlang::abort(sprintf(
      "Unsupported extension '.%s'. Use .dot, .gv, .svg, .png, or .pdf.", ext
    ))
  }
  for (pkg in c("DiagrammeR", "DiagrammeRsvg", "rsvg")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      rlang::abort(sprintf(
        "Package '%s' is required to export .%s files. Install it with install.packages('%s').",
        pkg, ext, pkg
      ))
    }
  }
  svg <- DiagrammeRsvg::export_svg(DiagrammeR::grViz(dot))
  if (ext == "svg") {
    writeLines(svg, file)
  } else if (ext == "png") {
    rsvg::rsvg_png(charToRaw(svg), file)
  } else {
    rsvg::rsvg_pdf(charToRaw(svg), file)
  }
  invisible(file)
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
graph_to_dot <- function(graph, show_col_edges = TRUE, show_legend = TRUE,
                         rank_lanes = TRUE, rankdir = c("LR", "TB")) {
  stopifnot(inherits(graph, "rdataflow_graph"))
  rankdir <- match.arg(rankdir)

  # Generate one DOT node statement per table node and stage node.
  tbl_stmts <- purrr::map_chr(
    seq_len(nrow(graph$table_nodes)),
    function(i) dot_table_node(graph$table_nodes[i, ])
  )
  stg_stmts <- purrr::map_chr(
    seq_len(nrow(graph$stage_nodes)),
    function(i) dot_stage_node(graph$stage_nodes[i, ])
  )

  # Edge statements differ by mode. In column-edge mode the structural
  # edges are still drawn (faintly, unlabelled ports) so join types and
  # table→stage relationships stay visible; without them a joined table
  # whose columns are all filters would float disconnected.
  edge_stmts <- if (show_col_edges) {
    c(
      build_source_edge_stmts(graph$source_edges, faint = TRUE),
      build_cte_edge_stmts(graph$cte_edges),
      build_temp_edge_stmts(graph$temp_edges),
      build_col_edge_stmts(graph$col_edges)
    )
  } else {
    c(
      build_source_edge_stmts(graph$source_edges),
      build_cte_edge_stmts(graph$cte_edges),
      build_temp_edge_stmts(graph$temp_edges)
    )
  }

  # Optional rank=same constraints to align parallel branches in columns.
  rank_stmts <- if (isTRUE(rank_lanes)) {
    dot_rank_constraints(compute_node_ranks(graph))
  } else {
    character(0)
  }

  # Optional legend cluster appended before the closing brace. The legend is
  # dynamic: only categories that actually occur in this graph are listed.
  legend_stmts <- if (isTRUE(show_legend)) {
    dot_legend_subgraph(graph, show_col_edges)
  } else {
    character(0)
  }

  paste(
    c(
      dot_preamble(rankdir), tbl_stmts, "", stg_stmts, "",
      rank_stmts, "",
      edge_stmts, legend_stmts, "}"
    ),
    collapse = "\n"
  )
}

# ---------------------------------------------------------------------------
# DOT preamble
# ---------------------------------------------------------------------------

dot_preamble <- function(rankdir = "LR") {
  c(
    "digraph sqlflow {",
    sprintf('  graph [rankdir=%s bgcolor="#fafafa" fontname="Helvetica" pad="0.4"',
            rankdir),
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
  n_hidden <- if ("n_hidden" %in% names(row)) row$n_hidden else 0L
  html  <- html_table_label(row$label, row$columns[[1]], n_hidden)
  sprintf('  %s [label=<%s>]', row$node_id, html)
}

# Produce a single DOT node statement for a stage (CTE / output) node row.
dot_stage_node <- function(row) {
  html <- html_stage_label(
    display_name    = row$display_name,
    role            = row$role,
    columns_tbl     = row$columns[[1]],
    transform_label = row$transform_label,
    where           = if ("where" %in% names(row)) row$where else NA_character_
  )
  sprintf('  %s [label=<%s>]', row$node_id, html)
}

# ---------------------------------------------------------------------------
# HTML label builders
# ---------------------------------------------------------------------------

# Build the HTML table label for a physical source table node.
# columns_tbl has cols: col_name, col_type, used, is_key. n_hidden > 0 adds
# an "… n more columns" overflow row (see build_graph(max_cols=)).
html_table_label <- function(table_label, columns_tbl, n_hidden = 0L) {
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

  # Overflow row when max_cols suppressed part of the catalog.
  overflow <- if (n_hidden > 0L) {
    sprintf(
      paste0(
        '<TR><TD COLSPAN="2" BGCOLOR="%s" ALIGN="LEFT">',
        '<FONT COLOR="#888888" POINT-SIZE="9"><I>&#8230; %d more columns</I></FONT>',
        '</TD></TR>'
      ),
      .col_none_bg, n_hidden
    )
  } else {
    NULL
  }

  rows_str <- paste(c(header, col_rows, overflow), collapse = "")
  sprintf(
    '<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="3">%s</TABLE>',
    rows_str
  )
}

# Build the HTML table label for a stage node.
# columns_tbl has cols: col_name, expr (optional), transform_type. A non-NA
# `where` renders as a footer row showing the stage's filter predicate.
html_stage_label <- function(display_name, role, columns_tbl, transform_label,
                             where = NA_character_) {
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

    # Hover tooltip carrying the full SELECT-list expression. Graphviz only
    # honours TOOLTIP on a cell that also has HREF; the SVG renderer turns
    # this into an anchor with a title, so hovering the cell shows exactly
    # what the column computes. Skipped for plain passthrough references
    # where the expression adds nothing.
    expr <- if ("expr" %in% names(col)) col$expr else NA_character_
    tooltip_attr <- if (!is.na(expr) && nzchar(expr) &&
                        !identical(tt, "passthrough")) {
      sprintf(' HREF="#" TOOLTIP="%s"', html_esc(expr))
    } else {
      ""
    }

    sprintf(
      paste0(
        '<TR>',
        '<TD PORT="%s" BGCOLOR="%s" ALIGN="LEFT"%s>%s</TD>',
        '<TD BGCOLOR="%s" ALIGN="LEFT">%s</TD>',
        '</TR>'
      ),
      pname, bg, tooltip_attr, html_esc(col$col_name), bg, type_html
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

  # Filter footer: the stage's WHERE predicate, truncated for readability
  # with the full text in a hover tooltip.
  where_footer <- if (!is.na(where) && nzchar(where)) {
    where_disp <- if (nchar(where) > 70L) paste0(substr(where, 1L, 67L), "...")
                  else where
    sprintf(
      paste0(
        '<TR><TD COLSPAN="2" BGCOLOR="%s" ALIGN="LEFT" HREF="#" TOOLTIP="%s">',
        '<FONT COLOR="#8a5a00" POINT-SIZE="9">WHERE %s</FONT>',
        '</TD></TR>'
      ),
      .transform_label_bg, html_esc(paste("WHERE", where)), html_esc(where_disp)
    )
  } else {
    NULL
  }

  rows_str <- paste(c(header, col_rows, footer, where_footer), collapse = "")
  sprintf(
    '<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="3">%s</TABLE>',
    rows_str
  )
}

# ---------------------------------------------------------------------------
# Edge statement builders
# ---------------------------------------------------------------------------

# Build DOT edge statements for column-level lineage (port-to-port edges).
# Each edge carries a hover tooltip describing the source → target columns.
build_col_edge_stmts <- function(col_edges) {
  if (nrow(col_edges) == 0) return(character(0))
  purrr::map_chr(seq_len(nrow(col_edges)), function(i) {
    row <- col_edges[i, ]
    fp  <- port_id(row$from_port)
    tp  <- port_id(row$to_port)
    tip <- dot_esc(sprintf("%s → %s", row$from_port, row$to_port))
    sprintf(
      '  %s:%s -> %s:%s [style=dashed color="#4a90d9" arrowsize=0.7 tooltip="%s"]',
      row$from_node_id, fp, row$to_node_id, tp, tip
    )
  })
}

# Build DOT edge statements for structural table→stage connections.
# join_type is NA for the primary FROM table and a string like "LEFT JOIN"
# for joined tables; join edges are labelled with the join type AND the key
# equalities (e.g. "LEFT JOIN\nr.region_id = c.region_id").
#
# faint = TRUE renders the structural skeleton underneath column-level
# lineage edges: lighter colour, no arrowheads competing with the lineage
# arrows, keys in the tooltip only (label stays short to reduce clutter).
build_source_edge_stmts <- function(source_edges, faint = FALSE) {
  if (nrow(source_edges) == 0) return(character(0))
  purrr::map_chr(seq_len(nrow(source_edges)), function(i) {
    row <- source_edges[i, ]
    keys <- if ("keys" %in% names(row)) as.character(unlist(row$keys)) else character(0)
    keys_txt <- paste(keys, collapse = "\n")

    if (!is.na(row$join_type)) {
      label <- if (faint || length(keys) == 0L) {
        row$join_type
      } else {
        paste0(row$join_type, "\n", keys_txt)
      }
      tip <- dot_esc(paste(c(row$join_type, keys), collapse = "\n"))
      color <- if (faint) "#e0b58a" else "#cc6600"
      sprintf(
        '  %s -> %s [label="%s" color="%s" fontcolor="%s" tooltip="%s"%s]',
        row$from_node_id, row$to_node_id,
        dot_esc(label), color, color, tip,
        if (faint) " arrowsize=0.6" else ""
      )
    } else if (faint) {
      sprintf('  %s -> %s [color="#cccccc" arrowsize=0.6 tooltip="FROM"]',
              row$from_node_id, row$to_node_id)
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
# Rank-lane computation
# ---------------------------------------------------------------------------

# Assign a topological depth to every node in the graph.
#
# Physical table nodes start at depth 0. Each stage node's depth is
# max(predecessor depths) + 1, where predecessors are found via source_edges,
# cte_edges, and temp_edges. The result is a named integer vector
# (node_id → depth). Nodes with unresolvable predecessors (shouldn't occur
# in a valid DAG) are left as NA and silently excluded from rank blocks.
compute_node_ranks <- function(graph) {
  # Collect all dependency edges from all three edge types.
  edge_sets <- list(
    graph$source_edges,
    graph$cte_edges,
    graph$temp_edges
  )
  edge_sets <- Filter(\(e) !is.null(e) && nrow(e) > 0L, edge_sets)

  all_edges <- if (length(edge_sets) > 0L) {
    purrr::list_rbind(
      purrr::map(edge_sets, \(e) e[, c("from_node_id", "to_node_id")])
    )
  } else {
    tibble::tibble(from_node_id = character(), to_node_id = character())
  }

  tbl_ids <- graph$table_nodes$node_id
  stg_ids <- graph$stage_nodes$node_id

  # Seed: physical tables at depth 0, stages unknown.
  ranks <- as.list(
    stats::setNames(
      c(rep(0L, length(tbl_ids)), rep(NA_integer_, length(stg_ids))),
      c(tbl_ids, stg_ids)
    )
  )

  # Iterative BFS: resolve each stage node once all its predecessors are known.
  # Runs at most length(stg_ids) passes; terminates early when nothing changes.
  for (pass in seq_along(stg_ids)) {
    changed <- FALSE
    for (nid in stg_ids) {
      if (!is.na(ranks[[nid]])) next  # already resolved

      preds <- all_edges$from_node_id[all_edges$to_node_id == nid]

      if (length(preds) == 0L) {
        # Stage with no identified predecessor (e.g. standalone SELECT) → depth 1.
        ranks[[nid]] <- 1L
        changed <- TRUE
      } else {
        pred_ranks <- vapply(preds, \(p) ranks[[p]] %||% NA_integer_, integer(1))
        if (anyNA(pred_ranks)) next  # predecessor not yet resolved; retry later
        ranks[[nid]] <- max(pred_ranks) + 1L
        changed <- TRUE
      }
    }
    if (!changed) break
  }

  unlist(ranks)
}

# Generate DOT rank=same constraint blocks from a named depth vector.
# Each unique depth gets one block listing all node IDs at that depth.
# Depth-0 nodes (physical tables) are excluded — Graphviz already places
# source nodes on the left naturally and adding rank=same there can confuse
# the layout when there are many tables at different vertical positions.
dot_rank_constraints <- function(node_ranks) {
  # Drop NAs and physical table nodes (depth 0) — constrain stages only.
  stage_ranks <- node_ranks[!is.na(node_ranks) & node_ranks > 0L]
  if (length(stage_ranks) == 0L) return(character(0))

  # One {rank=same; ...} block per depth level.
  rank_groups <- split(names(stage_ranks), stage_ranks)
  purrr::map_chr(
    rank_groups,
    \(node_ids) sprintf("  {rank=same; %s}", paste(node_ids, collapse = "; "))
  )
}

# ---------------------------------------------------------------------------
# Legend subgraph
# ---------------------------------------------------------------------------

# Build a Graphviz cluster subgraph containing a colour-coding legend.
# The legend is dynamic: only entries for categories that actually occur in
# `graph` are included, so a small diagram gets a small legend. Returns a
# character vector of DOT lines ready to splice into the graph body.
dot_legend_subgraph <- function(graph, show_col_edges = TRUE) {
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

  # --- What does this graph actually contain? -------------------------------
  tbl_cols <- purrr::list_rbind(graph$table_nodes$columns)
  has_tbl  <- nrow(graph$table_nodes) > 0
  has_cte  <- any(graph$stage_nodes$role == "cte")
  has_out  <- any(graph$stage_nodes$role == "output")

  role_rows <- c(
    if (has_tbl && any(tbl_cols$used & !tbl_cols$is_key))
      legend_swatch("Projected",       .col_used_bg),
    if (has_tbl && any(tbl_cols$is_key & !tbl_cols$used))
      legend_swatch("Join key only",   .col_key_bg),
    if (has_tbl && any(tbl_cols$used & tbl_cols$is_key))
      legend_swatch("Projected + key", .col_both_bg),
    if (has_tbl && any(!tbl_cols$used & !tbl_cols$is_key))
      legend_swatch("Unused",          .col_none_bg)
  )

  stg_cols <- purrr::list_rbind(graph$stage_nodes$columns)
  present_types <- if (nrow(stg_cols) > 0) {
    tt <- stg_cols$transform_type
    tt[is.na(tt)] <- "passthrough"
    # Preserve the canonical ordering from .transform_colors.
    names(.transform_colors)[names(.transform_colors) %in% unique(tt)]
  } else {
    character(0)
  }
  transform_rows <- purrr::map_chr(
    present_types,
    function(tp) legend_swatch(tp, .transform_colors[[tp]])
  )

  has_join_edge <- nrow(graph$source_edges) > 0 &&
    any(!is.na(graph$source_edges$join_type))
  has_from_edge <- nrow(graph$source_edges) > 0 &&
    any(is.na(graph$source_edges$join_type))
  edge_rows <- c(
    if (has_from_edge)
      legend_edge("Source / FROM",  "#888888", dashed = FALSE),
    if (has_join_edge)
      legend_edge("JOIN (keys on hover)", "#cc6600", dashed = FALSE),
    if (nrow(graph$cte_edges) > 0)
      legend_edge("CTE reference",  "#666666", dashed = TRUE),
    if (!is.null(graph$temp_edges) && nrow(graph$temp_edges) > 0)
      legend_edge("#temp feed",     "#4477AA", dashed = TRUE),
    if (show_col_edges && nrow(graph$col_edges) > 0)
      legend_edge("Column lineage", "#4a90d9", dashed = TRUE)
  )

  rows <- c(
    # Title
    paste0(
      '<TR><TD BGCOLOR="#555555" ALIGN="LEFT">',
      '<FONT COLOR="white" POINT-SIZE="10"><B>Legend</B></FONT>',
      '</TD></TR>'
    ),

    # Node header colours
    legend_section("Node headers"),
    if (has_tbl) legend_swatch("Physical table", .tbl_header_bg, "white"),
    if (has_cte) legend_swatch("CTE stage",      .cte_header_bg, "white"),
    if (has_out) legend_swatch("Output stage",   .out_header_bg, "white"),

    # Column role colours (table nodes) — only roles that occur
    if (length(role_rows) > 0) c(legend_section("Column role (table nodes)"),
                                 role_rows),

    # Transformation type colours (stage nodes) — only types that occur
    if (length(transform_rows) > 0)
      c(legend_section("Transformation (stage nodes)"), transform_rows),

    # Edge types — only styles that occur
    if (length(edge_rows) > 0) c(legend_section("Edges"), edge_rows)
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

# Escape a string for use inside a double-quoted DOT attribute value.
# Newlines become the DOT "\n" line-break escape.
dot_esc <- function(x) {
  x <- gsub("\\", "\\\\", as.character(x), fixed = TRUE)
  x <- gsub("\"", "\\\"", x, fixed = TRUE)
  gsub("\n", "\\n", x, fixed = TRUE)
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

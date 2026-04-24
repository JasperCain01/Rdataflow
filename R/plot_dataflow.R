#' Render a dataflow graph as a flow diagram
#'
#' Draws a dataflow graph (or the output of [trace_dataflow()]) as a
#' DiagrammeR flow diagram. Nodes are shaded by kind: imports are green
#' parallelograms, transformations are blue rectangles, and other nodes
#' are light grey.
#'
#' @param graph A list with `nodes` and `edges`, typically produced by
#'   [build_dataflow_graph()] or [trace_dataflow()].
#'
#' @return A `DiagrammeR` htmlwidget.
#' @export
plot_dataflow <- function(graph) {
  # Minimal shape check — we accept any list with a `nodes` tibble and
  # an `edges` tibble, so `plot_dataflow()` can be used both on the
  # full graph and on lineage subsets returned by trace_lineage().
  stopifnot(all(c("nodes", "edges") %in% names(graph)))

  # Build one DOT line per node. Imports are drawn as green
  # parallelograms (evoking the classic "data source" flowchart shape)
  # and transformations as blue rectangles so the visual hierarchy of
  # the dataflow reads as "source -> transform -> transform -> output".
  node_lines <- graph$nodes |>
    dplyr::mutate(
      # Shape is driven by kind — imports use the data-source
      # parallelogram, everything else uses a process rectangle.
      shape = dplyr::case_when(
        .data$kind == "import" ~ "parallelogram",
        .data$kind == "transform" ~ "rectangle",
        TRUE ~ "rectangle"
      ),
      # Fill colour mirrors the kind classification; the palette is
      # chosen from Material-ish pastels so the diagram stays readable.
      fill = dplyr::case_when(
        .data$kind == "import" ~ "#C8E6C9",
        .data$kind == "transform" ~ "#BBDEFB",
        TRUE ~ "#ECEFF1"
      ),
      # Compose the multi-line label: target name, headline function,
      # human summary of the transformation, and the column names of
      # the resulting object (when we know them). `build_node_label`
      # drops sections that are empty so sparse nodes stay compact.
      label = purrr::pmap_chr(
        list(.data$id, .data$call_fn, .data$summary, .data$columns),
        build_node_label
      ),
      # Assemble the Graphviz node line. `escape_label` handles the
      # quoting / newline escaping so the multi-line label renders
      # correctly in Graphviz.
      dot = sprintf(
        '  "%s" [label="%s", shape=%s, style=filled, fillcolor="%s"];',
        .data$id,
        escape_label(.data$label),
        .data$shape,
        .data$fill
      )
    ) |>
    dplyr::pull("dot")

  # Build one DOT line per edge. We special-case the empty-edge case
  # because mutate on a zero-row tibble still works, but skipping the
  # pipeline keeps the DOT output tidy for single-node lineages.
  edge_lines <- if (nrow(graph$edges) == 0) {
    character()
  } else {
    graph$edges |>
      dplyr::mutate(
        dot = sprintf('  "%s" -> "%s";', .data$from, .data$to)
      ) |>
      dplyr::pull("dot")
  }

  # Stitch the header, node lines, and edge lines into a single DOT
  # document. `rankdir=LR` lays the graph left-to-right, which matches
  # how users read a data pipeline (source on the left, result on the
  # right). Helvetica is picked for cross-platform legibility.
  dot <- paste(
    c(
      "digraph dataflow {",
      "  rankdir=LR;",
      "  node [fontname=\"Helvetica\"];",
      node_lines,
      edge_lines,
      "}"
    ),
    collapse = "\n"
  )

  # Hand the assembled DOT to DiagrammeR, which returns an htmlwidget
  # that renders in the RStudio Viewer, in R Markdown, or standalone.
  DiagrammeR::grViz(dot)
}

# Escape characters that would break the DOT string literal: literal
# double-quotes and embedded newlines. Using stringr keeps the logic
# pipe-friendly and consistent with the rest of the package.
escape_label <- function(x) {
  x |>
    stringr::str_replace_all('"', '\\\\"') |>
    stringr::str_replace_all("\n", "\\\\n")
}

# Compose a node's multi-line label from its parts. Sections with no
# information are skipped so, for example, an "other" assignment with
# no summary and no columns just shows its name. Columns are wrapped
# across lines and truncated with a "... +N more" tail so wide tables
# do not blow up the diagram.
build_node_label <- function(id, call_fn, summary, columns) {
  parts <- id
  if (!is.na(call_fn) && nzchar(call_fn)) {
    parts <- c(parts, call_fn)
  }
  if (!is.null(summary) && length(summary) == 1 && nzchar(summary)) {
    parts <- c(parts, summary)
  }
  if (length(columns) > 0) {
    parts <- c(parts, format_columns_line(columns))
  }
  paste(parts, collapse = "\n")
}

# Render a column list into one or two lines, capping the number of
# names shown so the node stays readable. The cap is loose enough to
# show most real tables in full while bounded enough to prevent a wide
# table from dominating the diagram.
format_columns_line <- function(columns, max_cols = 10L, per_line = 5L) {
  n <- length(columns)
  shown <- columns[seq_len(min(n, max_cols))]
  # Group the shown names into chunks of `per_line` so a line break is
  # inserted roughly every few columns.
  chunks <- split(shown, ceiling(seq_along(shown) / per_line))
  body <- paste(purrr::map_chr(chunks, paste, collapse = ", "),
                collapse = "\n")
  prefix <- "cols: "
  if (n > max_cols) {
    sprintf("%s%s\n(+%d more)", prefix, body, n - max_cols)
  } else {
    paste0(prefix, body)
  }
}

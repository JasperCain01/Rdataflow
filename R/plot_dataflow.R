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
      # Assemble the Graphviz node line. `escape_label` handles the
      # quoting / newline escaping so node labels can carry a
      # line-break between the target name and the function name.
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

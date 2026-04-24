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
  stopifnot(all(c("nodes", "edges") %in% names(graph)))

  node_lines <- graph$nodes |>
    dplyr::mutate(
      shape = dplyr::case_when(
        .data$kind == "import" ~ "parallelogram",
        .data$kind == "transform" ~ "rectangle",
        TRUE ~ "rectangle"
      ),
      fill = dplyr::case_when(
        .data$kind == "import" ~ "#C8E6C9",
        .data$kind == "transform" ~ "#BBDEFB",
        TRUE ~ "#ECEFF1"
      ),
      dot = sprintf(
        '  "%s" [label="%s", shape=%s, style=filled, fillcolor="%s"];',
        .data$id,
        escape_label(.data$label),
        .data$shape,
        .data$fill
      )
    ) |>
    dplyr::pull("dot")

  edge_lines <- if (nrow(graph$edges) == 0) {
    character()
  } else {
    graph$edges |>
      dplyr::mutate(
        dot = sprintf('  "%s" -> "%s";', .data$from, .data$to)
      ) |>
      dplyr::pull("dot")
  }

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

  DiagrammeR::grViz(dot)
}

escape_label <- function(x) {
  x |>
    stringr::str_replace_all('"', '\\\\"') |>
    stringr::str_replace_all("\n", "\\\\n")
}

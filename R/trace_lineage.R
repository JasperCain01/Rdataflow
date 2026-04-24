#' Trace the lineage of a target object within a dataflow graph
#'
#' Walks the graph backwards from `target` and returns the subgraph containing
#' only nodes and edges that contribute (directly or transitively) to the
#' target.
#'
#' @param graph A list with `nodes` and `edges`, as returned by
#'   [build_dataflow_graph()].
#' @param target The name of the object whose lineage should be traced.
#'
#' @return A list of the same shape as `graph`, subset to the target's ancestry.
#' @keywords internal
trace_lineage <- function(graph, target) {
  stopifnot(target %in% graph$nodes$id)

  ancestors <- target
  frontier <- target

  while (length(frontier) > 0) {
    parents <- graph$edges |>
      dplyr::filter(.data$to %in% frontier) |>
      dplyr::pull("from")

    new_parents <- setdiff(parents, ancestors)
    ancestors <- c(ancestors, new_parents)
    frontier <- new_parents
  }

  list(
    nodes = dplyr::filter(graph$nodes, .data$id %in% ancestors),
    edges = dplyr::filter(
      graph$edges,
      .data$from %in% ancestors,
      .data$to %in% ancestors
    )
  )
}

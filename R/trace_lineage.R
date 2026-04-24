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
  # Require the target to be a node in the graph; otherwise a call
  # like `trace_dataflow(typo_name, ...)` would silently return empty.
  stopifnot(target %in% graph$nodes$id)

  # Breadth-first walk backwards through the DAG. `ancestors` is the
  # accumulated set of nodes reachable by going upstream from `target`;
  # `frontier` holds the newly-added nodes whose parents we still need
  # to expand. The loop terminates when we stop discovering new ones.
  ancestors <- target
  frontier <- target

  while (length(frontier) > 0) {
    # Find every node that points into something in the current
    # frontier — these are the next layer of ancestors.
    parents <- graph$edges |>
      dplyr::filter(.data$to %in% frontier) |>
      dplyr::pull("from")

    # Only keep parents we have not already recorded; this both
    # prevents duplicate work and guarantees termination on cyclic or
    # self-referencing inputs (should any slip past the edge filter).
    new_parents <- setdiff(parents, ancestors)
    ancestors <- c(ancestors, new_parents)
    frontier <- new_parents
  }

  # Induce the subgraph: keep only nodes in `ancestors` and only edges
  # whose endpoints are both in that set. This is what downstream
  # rendering should draw for the user's object.
  list(
    nodes = dplyr::filter(graph$nodes, .data$id %in% ancestors),
    edges = dplyr::filter(
      graph$edges,
      .data$from %in% ancestors,
      .data$to %in% ancestors
    )
  )
}

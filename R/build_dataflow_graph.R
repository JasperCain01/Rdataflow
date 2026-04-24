#' Build a dataflow graph from a parsed script
#'
#' Converts the output of [parse_script()] into a pair of tibbles
#' (`nodes`, `edges`) describing the dataflow as a directed acyclic graph.
#' Each assignment becomes a node labelled with its target and the function
#' that produced it. Edges connect inputs (only those that correspond to
#' previously-defined variables) to the node that consumes them.
#'
#' @param parsed A tibble returned by [parse_script()].
#'
#' @return A list with elements `nodes` and `edges` (both tibbles).
#' @export
build_dataflow_graph <- function(parsed) {
  # Guard: the rest of the function assumes the input is the tibble
  # returned by parse_script(); stop early with a clear failure if not.
  stopifnot(tibble::is_tibble(parsed))

  # Turn each parsed assignment into a graph node. The `label` combines
  # the target name with the headline function so that diagram nodes
  # read as "what was produced / how it was produced".
  nodes <- parsed |>
    dplyr::transmute(
      id = .data$target,
      step = .data$step,
      label = paste0(.data$target, "\n", dplyr::coalesce(.data$call_fn, "")),
      kind = .data$kind,
      rhs_code = .data$rhs_code,
      call_fn = .data$call_fn
    )

  # Set of names that correspond to assigned objects in this script —
  # used below to drop symbol references that point to arguments,
  # function names from other packages, inline constants, etc.
  known <- nodes$id

  # Explode the `inputs` list column so each (target, input) pair
  # becomes one row, then keep only pairs where the input is itself
  # a previously-defined target (i.e. a real edge in the DAG). The
  # self-edge filter protects against quirks where a variable is
  # referenced on its own RHS (e.g. `x <- x + 1` style updates).
  edges <- parsed |>
    dplyr::select("target", "inputs") |>
    tidyr::unnest_longer("inputs") |>
    dplyr::filter(.data$inputs %in% known, .data$inputs != .data$target) |>
    dplyr::transmute(from = .data$inputs, to = .data$target) |>
    dplyr::distinct()

  # Return both tibbles together — downstream functions (trace_lineage,
  # plot_dataflow) consume the pair as a single graph object.
  list(nodes = nodes, edges = edges)
}

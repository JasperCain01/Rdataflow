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
  stopifnot(tibble::is_tibble(parsed))

  nodes <- parsed |>
    dplyr::transmute(
      id = .data$target,
      step = .data$step,
      label = paste0(.data$target, "\n", dplyr::coalesce(.data$call_fn, "")),
      kind = .data$kind,
      rhs_code = .data$rhs_code,
      call_fn = .data$call_fn
    )

  known <- nodes$id

  edges <- parsed |>
    dplyr::select("target", "inputs") |>
    tidyr::unnest_longer("inputs") |>
    dplyr::filter(.data$inputs %in% known, .data$inputs != .data$target) |>
    dplyr::transmute(from = .data$inputs, to = .data$target) |>
    dplyr::distinct()

  list(nodes = nodes, edges = edges)
}

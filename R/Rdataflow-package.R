#' Rdataflow: Reverse-Engineer and Visualize Data Flow in R Scripts
#'
#' Given an R object and the script that produced it, Rdataflow traces the
#' assignments and transformations that led to the object and renders the
#' lineage as a flow diagram.
#'
#' The high-level entry point is [trace_dataflow()]. Lower-level building
#' blocks are [parse_script()], [build_dataflow_graph()], and
#' [plot_dataflow()].
#'
#' @keywords internal
#' @importFrom rlang .data
"_PACKAGE"

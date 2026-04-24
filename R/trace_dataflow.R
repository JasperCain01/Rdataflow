#' Trace and visualize the dataflow for an R object
#'
#' Given an object (or the name of an object) and the script that produced
#' it, `trace_dataflow()` parses the script, builds a dataflow graph, and
#' returns either the lineage graph for that object or a rendered flow
#' diagram.
#'
#' @param object The object whose lineage to trace. May be passed unquoted
#'   (e.g. `my_data`), as a string (`"my_data"`), or as a variable name.
#' @param script Path to the R script that produced the object, or a
#'   character vector of R source code.
#' @param plot If `TRUE` (the default) return a flow diagram; otherwise
#'   return the lineage graph as a list of `nodes`/`edges` tibbles.
#'
#' @return Either a `DiagrammeR` htmlwidget or a list of tibbles.
#' @export
#'
#' @examples
#' \dontrun{
#'   trace_dataflow(model_data, script = "analysis.R")
#' }
trace_dataflow <- function(object, script, plot = TRUE) {
  target <- resolve_target_name(rlang::enquo(object))

  parsed <- parse_script(script)
  graph <- build_dataflow_graph(parsed)

  if (!target %in% graph$nodes$id) {
    rlang::abort(sprintf(
      "Could not find an assignment for '%s' in the supplied script.",
      target
    ))
  }

  lineage <- trace_lineage(graph, target)

  if (plot) plot_dataflow(lineage) else lineage
}

resolve_target_name <- function(quo) {
  expr <- rlang::quo_get_expr(quo)
  if (is.character(expr)) return(expr)
  if (is.symbol(expr)) return(as.character(expr))

  val <- try(rlang::eval_tidy(quo), silent = TRUE)
  if (is.character(val) && length(val) == 1) return(val)

  rlang::abort(
    "`object` must be a symbol, a single string, or evaluate to one."
  )
}

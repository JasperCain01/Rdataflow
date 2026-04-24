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
#' @param env Environment in which to look up object column names for
#'   display on the diagram. Defaults to the caller's frame so users
#'   who have already run the script get column annotations for free;
#'   pass `NULL` to skip the lookup entirely.
#'
#' @return Either a `DiagrammeR` htmlwidget or a list of tibbles. The
#'   nodes tibble includes `summary` (the transformation description)
#'   and `columns` (the names of the columns in the object, when it is
#'   available in `env`).
#' @export
#'
#' @examples
#' \dontrun{
#'   trace_dataflow(model_data, script = "analysis.R")
#' }
trace_dataflow <- function(object, script, plot = TRUE,
                           env = parent.frame()) {
  # Capture the `object` argument without evaluating it so the user
  # can pass a bare symbol (`trace_dataflow(model_data, ...)`) without
  # needing the object to exist in the calling environment.
  target <- resolve_target_name(rlang::enquo(object))

  # Pipeline: read the script -> tabulate assignments -> link them
  # up into a DAG. Doing this eagerly (rather than lazily) means any
  # parse failure surfaces here instead of during rendering.
  parsed <- parse_script(script)
  graph <- build_dataflow_graph(parsed)

  # Fail fast with a helpful message if the requested target was never
  # assigned in the script — silently returning an empty plot is worse
  # than a clear error at the top level.
  if (!target %in% graph$nodes$id) {
    rlang::abort(sprintf(
      "Could not find an assignment for '%s' in the supplied script.",
      target
    ))
  }

  # Walk the DAG backwards from `target` so the returned graph only
  # contains nodes and edges that actually contribute to it.
  lineage <- trace_lineage(graph, target)

  # Best-effort column annotation: if the user has already run the
  # script, we look up each lineage node in `env` and attach the
  # object's column names for display in the diagram. Missing objects
  # simply get an empty column vector.
  lineage <- attach_columns(lineage, env)

  # Default is to return a ready-to-view diagram; `plot = FALSE` is
  # useful for programmatic inspection or for writing tests.
  if (plot) plot_dataflow(lineage) else lineage
}

# Turn the captured `object` argument into the single string name we
# need for graph lookup. We accept:
#   * a bare symbol, e.g. `model_data`            -> "model_data"
#   * a literal string, e.g. `"model_data"`       -> "model_data"
#   * any expression evaluating to a single string, e.g. `varname`
# Anything else is an error because we cannot reliably derive a name.
resolve_target_name <- function(quo) {
  expr <- rlang::quo_get_expr(quo)
  # Literal string in the call: use it directly.
  if (is.character(expr)) return(expr)
  # Bare symbol: convert to its character representation.
  if (is.symbol(expr)) return(as.character(expr))

  # Fallback: evaluate the expression in its captured environment and
  # accept the result only if it is a single-string character vector.
  val <- try(rlang::eval_tidy(quo), silent = TRUE)
  if (is.character(val) && length(val) == 1) return(val)

  rlang::abort(
    "`object` must be a symbol, a single string, or evaluate to one."
  )
}

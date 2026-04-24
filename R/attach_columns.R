#' Attach column names from a live environment to graph nodes
#'
#' For each node in a graph, look up the object of the same name in the
#' supplied environment and, if it is a data frame, record its column
#' names on the node. Nodes whose objects are missing or not data-frame
#' like retain an empty `columns` vector.
#'
#' @param graph A list with `nodes` and `edges`, as returned by
#'   [build_dataflow_graph()] or [trace_lineage()].
#' @param env An environment in which to look up object names.
#'
#' @return The same graph with populated `columns` list column on
#'   `nodes`.
#' @keywords internal
attach_columns <- function(graph, env) {
  # If no environment is provided there is nothing to look up; return
  # the graph unchanged so callers can opt out transparently.
  if (is.null(env)) return(graph)

  # Walk each node and try to find the named object in `env`. Objects
  # that do not exist, or that are not tibble / data.frame-like, leave
  # the node's `columns` untouched (defaults to character(0)).
  graph$nodes <- graph$nodes |>
    dplyr::mutate(
      columns = purrr::map(.data$id, function(name) {
        # `exists` is used with `inherits = TRUE` so parent frames can
        # satisfy the lookup — helpful when the user runs the script
        # in globalenv() and calls us from a helper function.
        if (!exists(name, envir = env, inherits = TRUE)) {
          return(character())
        }
        obj <- get(name, envir = env, inherits = TRUE)
        # Tibbles inherit from data.frame, so this check covers both.
        if (is.data.frame(obj)) {
          names(obj)
        } else {
          character()
        }
      })
    )

  graph
}

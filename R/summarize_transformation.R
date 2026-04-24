#' Build a short human-readable summary of an assignment's RHS
#'
#' Produces a one-line description of the transformation(s) applied on the
#' right-hand side of an assignment. Piped chains (native `|>` and
#' `%>%`) are unpacked so each verb in the chain is described in order,
#' joined with `" -> "`.
#'
#' @param rhs An R expression (the right-hand side of an assignment).
#'
#' @return A length-1 character vector summarising the transformation.
#' @keywords internal
summarize_transformation <- function(rhs) {
  # Break the RHS into an ordered list of "steps" — one per verb in
  # a pipe chain — so a single-line summary can narrate each stage of
  # the transformation instead of only the outermost call.
  steps <- extract_pipe_steps(rhs)

  # Describe each step and glue them with a right-arrow so that the
  # summary reads in the same direction the data flows.
  summaries <- purrr::map_chr(steps, summarize_single_call)
  summaries <- summaries[nzchar(summaries)]
  paste(summaries, collapse = " -> ")
}

# Unpack a piped expression into a list of steps, left-to-right.
# Handles the magrittr pipe (an explicit `%>%` call tree) and the
# native pipe (which is rewritten at parse time into nested calls,
# and is only treated as a pipe chain here when the outer call is a
# known tidyverse verb — otherwise arbitrary nesting would be
# misreported as piping).
extract_pipe_steps <- function(expr) {
  if (is_magrittr_pipe(expr)) {
    # %>% is parsed as `%>%(lhs, rhs)` — recurse on the lhs and append
    # the rhs as the next verb applied.
    c(extract_pipe_steps(expr[[2]]), list(expr[[3]]))
  } else if (is_tidyverb_chain(expr)) {
    # Nested dplyr/tidyr call like `mutate(filter(raw, ...), ...)` —
    # peel the first argument (which is itself a call) into a step.
    inner <- expr[[2]]
    outer <- expr
    outer[[2]] <- quote(.)
    c(extract_pipe_steps(inner), list(outer))
  } else {
    # Not a pipe / peelable chain — treat the whole thing as one step.
    list(expr)
  }
}

# TRUE iff `expr` is a `%>%` call. Guards check length and head shape
# first so we can safely inspect `expr[[1]]`.
is_magrittr_pipe <- function(expr) {
  is.call(expr) && length(expr) >= 3 && is.symbol(expr[[1]]) &&
    identical(as.character(expr[[1]]), "%>%")
}

# TRUE iff `expr` is a call to a known tidyverse verb whose first
# argument is itself a call — i.e. a pipe chain that the native `|>`
# left behind as nested calls after parsing.
is_tidyverb_chain <- function(expr) {
  if (!is.call(expr) || length(expr) < 2) return(FALSE)
  if (!is.call(expr[[2]])) return(FALSE)
  fn <- unqualified_fn_name(expr[[1]])
  !is.null(fn) && fn %in% tidy_verbs
}

# Resolve the head of a call to a plain function name, whether it is
# a bare symbol (`filter`) or a namespaced call (`dplyr::filter`).
unqualified_fn_name <- function(head) {
  # Bare symbol — easy case.
  if (is.symbol(head)) return(as.character(head))

  # Namespaced call `pkg::fn` or `pkg:::fn` — return the unqualified
  # `fn` part. Guard `is.symbol(head[[1]])` so we do not accidentally
  # treat a non-symbol head as a namespace operator.
  if (is.call(head) && length(head) >= 3 && is.symbol(head[[1]]) &&
        as.character(head[[1]]) %in% c("::", ":::")) {
    return(as.character(head[[3]]))
  }

  NULL
}

# Describe one call in a pipe chain. We handle the most common
# tidyverse verbs explicitly so the summary reads naturally (e.g.
# "filter: x > 0") and fall back to a generic "<fn>(<first args>)"
# form for anything else.
summarize_single_call <- function(expr) {
  if (!is.call(expr)) {
    # A bare seed value in a pipe chain — skip (we do not summarise
    # raw data; it becomes a node of its own).
    return("")
  }

  fn <- unqualified_fn_name(expr[[1]]) %||% "?"
  args <- as.list(expr)[-1]

  # Drop the `.` placeholder we inserted when peeling a native-pipe
  # chain; it would otherwise clutter every summary.
  if (length(args) > 0 && identical(args[[1]], quote(.))) {
    args <- args[-1]
  }

  # For tidyverse verbs the first positional argument is conventionally
  # the data frame being operated on, and it appears as a node of its
  # own in the graph, so including it in the summary would just repeat
  # information already visible in the upstream edge.
  if (fn %in% tidy_verbs && length(args) > 0) {
    nms <- names(args) %||% rep("", length(args))
    if (!nzchar(nms[[1]])) args <- args[-1]
  }

  # Fetch the argument descriptor specific to this verb. The helper
  # returns "" when nothing informative can be said about the args.
  body <- describe_args(fn, args)
  if (nzchar(body)) sprintf("%s: %s", fn, body) else fn
}

# Verb-specific descriptions. Grouped by argument style:
# - `named_verbs` expose new columns as name=expr pairs; we list the
#    names (or the bare expression if unnamed).
# - positional-verb families list the raw expressions.
# - joins call out the `by` key (if any).
# - imports display the path / connection expression.
describe_args <- function(fn, args) {
  named_verbs <- c("mutate", "transmute", "summarise", "summarize",
                   "rename", "add_column")
  positional_verbs <- c("filter", "select", "arrange", "group_by",
                        "distinct", "slice", "count", "drop_na")
  join_verbs <- c("left_join", "right_join", "inner_join", "full_join",
                  "anti_join", "semi_join")

  if (fn %in% named_verbs) {
    # For mutate-style verbs, prefer the output column names. For
    # unnamed arguments (e.g. `rename_with()` style), fall back to the
    # raw expression so the summary still conveys intent.
    nms <- names(args) %||% rep("", length(args))
    parts <- purrr::map2_chr(args, nms, function(a, n) {
      if (nzchar(n)) n else deparse_compact(a)
    })
    truncate_summary(paste(parts, collapse = ", "))

  } else if (fn %in% positional_verbs) {
    truncate_summary(paste(purrr::map_chr(args, deparse_compact),
                           collapse = ", "))

  } else if (fn %in% join_verbs) {
    # Report the join key (the `by` arg) when present; otherwise note
    # that it is a natural join on shared column names.
    by_arg <- args$by
    if (!is.null(by_arg)) {
      sprintf("by %s", truncate_summary(deparse_compact(by_arg)))
    } else {
      "natural"
    }

  } else if (fn %in% import_fns) {
    # For imports the first positional arg is typically the data
    # source — surface it verbatim so users can see what was read.
    if (length(args) > 0) truncate_summary(deparse_compact(args[[1]])) else ""

  } else {
    # Generic fallback: show up to the first two unnamed arguments.
    nms <- names(args) %||% rep("", length(args))
    unnamed <- args[!nzchar(nms)]
    if (length(unnamed) == 0) return("")
    keep <- unnamed[seq_len(min(2L, length(unnamed)))]
    truncate_summary(paste(purrr::map_chr(keep, deparse_compact),
                           collapse = ", "))
  }
}

# The set of tidyverse verbs that get treated as pipe steps when
# encountered as nested calls. Keeping this as a package-level vector
# (rather than re-declaring per call) keeps the hot path cheap.
tidy_verbs <- c(
  "filter", "mutate", "transmute", "summarise", "summarize",
  "select", "rename", "arrange", "group_by", "ungroup", "distinct",
  "slice", "count", "tally", "add_row", "add_column",
  "left_join", "right_join", "inner_join", "full_join",
  "anti_join", "semi_join",
  "pivot_longer", "pivot_wider", "separate", "unite",
  "drop_na", "fill", "complete", "bind_rows", "bind_cols",
  "rowwise", "nest", "unnest"
)

# Compact, single-line deparse for use inside summaries. The default
# deparse can line-wrap long expressions; we join those lines with a
# single space so the summary stays on one line.
deparse_compact <- function(x) {
  paste(deparse(x, width.cutoff = 500L), collapse = " ")
}

# Trim a summary fragment to a bounded length so diagram labels stay
# legible. We leave room for the verb prefix that will be added.
truncate_summary <- function(s, n = 60L) {
  if (nchar(s) > n) paste0(substr(s, 1L, n - 1L), "...") else s
}

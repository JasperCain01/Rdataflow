# ---------------------------------------------------------------------------
# Transformation classifier
#
# classify_transform() annotates the IR with the *kind* of transformation each
# stage applies. This drives the boxes drawn on the flow lines between stages
# (aggregations, date-diffs, counts, window functions, CASE expressions, ...).
#
# It adds two things to an rdataflow_ir:
#   * projections$transform_type - a per-output-column category
#   * $stage_transforms          - a per-stage summary tibble with structural
#                                  flags and a human-readable label
# ---------------------------------------------------------------------------

# Function-name groupings used to categorise a projection. Names are compared
# upper-case because the parser upper-cases them. These are intentionally
# T-SQL leaning (DATEDIFF/CONVERT/CHARINDEX, ...).
date_fns <- c("DATEDIFF", "DATEADD", "DATEPART", "DATENAME", "DATEFROMPARTS",
              "EOMONTH", "DATETRUNC", "GETDATE", "CURRENT_TIMESTAMP", "SYSDATETIME")
cast_fns <- c("CAST", "CONVERT", "TRY_CAST", "TRY_CONVERT")
string_fns <- c("CONCAT", "CONCAT_WS", "SUBSTRING", "LEFT", "RIGHT", "UPPER",
                "LOWER", "TRIM", "LTRIM", "RTRIM", "REPLACE", "LEN", "LENGTH",
                "CHARINDEX", "PATINDEX", "STUFF", "FORMAT", "REPLICATE")
# Aggregate function names, used to label which aggregates a stage uses (the
# is_aggregate flag already tells us *that* it aggregates).
agg_fns <- c("SUM", "COUNT", "COUNT_IF", "AVG", "MIN", "MAX", "STDEV", "STDEVP",
             "VAR", "VARP", "STRING_AGG", "COUNTDISTINCT")

#' Classify the transformations in a lineage IR
#'
#' @param ir An object of class `rdataflow_ir` (see [build_ir()]).
#'
#' @return The same IR with a `transform_type` column added to `projections`
#'   and a new `stage_transforms` summary tibble.
#' @export
classify_transform <- function(ir) {
  stopifnot(inherits(ir, "rdataflow_ir"))

  # --- per-projection category --------------------------------------------
  ir$projections <- ir$projections |>
    dplyr::mutate(
      transform_type = purrr::pmap_chr(
        list(.data$is_aggregate, .data$has_window, .data$has_case,
             .data$functions, .data$expr),
        classify_projection
      )
    )

  # --- per-stage summary ---------------------------------------------------
  ir$stage_transforms <- summarize_stage_transforms(ir)

  ir
}

# Categorise a single projection. Priority order matters: a window aggregate
# is reported as a window function; an aggregate over a date-diff is reported
# as an aggregate; and so on from most to least specific.
classify_projection <- function(is_aggregate, has_window, has_case, functions,
                                 expr) {
  funcs <- toupper(as.character(unlist(functions)))

  if (isTRUE(has_window)) return("window")
  if (isTRUE(is_aggregate)) return("aggregate")
  if (any(funcs %in% date_fns)) return("date")
  if (isTRUE(has_case)) return("case")
  if (any(funcs %in% cast_fns)) return("cast")
  if (any(funcs %in% string_fns)) return("string")

  # No recognised function: distinguish a plain arithmetic expression from a
  # straight passthrough of a column. We strip the trailing "AS alias" before
  # looking for arithmetic operators so the alias cannot trigger a match.
  body <- stringr::str_remove(expr, "(?i)\\s+AS\\s+\\[?[^\\]]+\\]?\\s*$")
  if (stringr::str_detect(body, "[-+*/%]")) return("arithmetic")
  if (length(funcs) > 0) return("expression")
  "passthrough"
}

# Build the per-stage transformation summary. For each stage we record which
# transformation kinds are present, the notable function names, and a compact
# label suitable for drawing on the flow line into that stage.
summarize_stage_transforms <- function(ir) {
  projections <- ir$projections
  group_by <- ir$group_by

  # Iterate stages in IR order so the summary aligns with the stage table.
  purrr::map(ir$stages$stage_id, function(sid) {
    proj <- projections[projections$stage_id == sid, , drop = FALSE]
    grp <- group_by[group_by$stage_id == sid, , drop = FALSE]

    # Which categories appear among this stage's projections?
    types <- proj$transform_type
    funcs <- toupper(as.character(unlist(proj$functions)))

    has_aggregation <- any(types == "aggregate") || nrow(grp) > 0
    has_window <- any(types == "window")
    has_case <- any(types == "case")
    has_date <- any(types == "date")

    tibble::tibble(
      stage_id = sid,
      has_aggregation = has_aggregation,
      has_window = has_window,
      has_case = has_case,
      has_date = has_date,
      # Notable function names actually used (aggregates + dates), de-duped.
      functions = list(unique(funcs[funcs %in% c(agg_fns, date_fns)])),
      label = build_stage_label(types, funcs, grp$expr)
    )
  }) |> purrr::list_rbind()
}

# Compose the flow-line box text for a stage from its projection categories,
# the functions it uses, and its GROUP BY columns.
build_stage_label <- function(types, funcs, group_exprs) {
  parts <- character()

  # Lead with grouping, since it frames how the aggregates are computed.
  if (length(group_exprs) > 0) {
    cols <- vapply(group_exprs, clean_identifier, character(1))
    parts <- c(parts, paste0("GROUP BY ", paste(unique(cols), collapse = ", ")))
  }

  # Name the specific aggregate functions used (SUM, COUNT, ...).
  used_aggs <- unique(funcs[funcs %in% agg_fns])
  if (length(used_aggs) > 0) {
    parts <- c(parts, paste(used_aggs, collapse = ", "))
  }

  # Call out other notable transformation kinds present in the stage.
  if (any(types == "window")) parts <- c(parts, "window fn")
  if (any(types == "date")) parts <- c(parts, "date calc")
  if (any(types == "case")) parts <- c(parts, "CASE")
  if (any(types == "cast")) parts <- c(parts, "cast")

  paste(parts, collapse = "; ")
}

# Reduce a (possibly bracketed, alias-qualified) column reference to its bare
# column name for compact labels, e.g. "[o].[customer_id]" -> "customer_id".
clean_identifier <- function(x) {
  # Take the part after the last dot, then strip surrounding brackets.
  last <- stringr::str_replace(x, "^.*\\.", "")
  stringr::str_replace(last, "^\\[(.*)\\]$", "\\1")
}

# ---------------------------------------------------------------------------
# Lineage intermediate representation (IR)
#
# build_ir() turns the nested, statement-oriented structure returned by
# parse_sql() into a set of tidy, typed tibbles wrapped in an "rdataflow_ir"
# S3 object. Flattening into relational tibbles makes the downstream graph
# assembly and rendering simple dplyr joins rather than recursive list walks.
#
# The IR holds these tibbles (all keyed by integer `stage_id`):
#   stages       - one row per stage (CTE / output) across all statements
#   projections  - one row per output column of a stage
#   proj_sources - column-level lineage: output column <- source column
#   sources      - the tables / upstream stages a stage reads from
#   joins        - one row per JOIN within a stage
#   join_keys    - one row per column=column equality in a join's ON
#   group_by     - one row per GROUP BY expression
# ---------------------------------------------------------------------------

#' Build the lineage IR from parsed SQL
#'
#' @param parsed The list returned by [parse_sql()].
#'
#' @return An object of class `rdataflow_ir` (a list of tibbles; see file
#'   header for the schema).
#' @export
build_ir <- function(parsed) {
  stopifnot(is.list(parsed), !is.null(parsed$statements))

  # Accumulators for each tibble. We append per stage and bind at the end;
  # binding a list of tibbles preserves column types even when some are empty.
  stages <- list()
  projections <- list()
  proj_sources <- list()
  sources <- list()
  joins <- list()
  join_keys <- list()
  group_by <- list()

  # Global stage counter so stage_id is unique across all statements.
  sid <- 0L

  for (st in parsed$statements) {
    for (stg in st$stages) {
      sid <- sid + 1L

      # Map each source alias to its underlying table name so projection
      # source columns (which reference aliases) can be resolved to real
      # tables / upstream stage names.
      alias_to_table <- source_alias_map(stg$sources)

      # --- stage row ---------------------------------------------------------
      stages[[length(stages) + 1L]] <- tibble::tibble(
        stage_id = sid,
        statement_index = as.integer(st$index),
        name = scalar_chr(stg$name),
        role = scalar_chr(stg$role),
        statement_kind = scalar_chr(st$kind),
        output_table = scalar_chr(st$output_table),
        is_output = identical(stg$role, "output")
      )

      # --- sources -----------------------------------------------------------
      for (src in stg$sources) {
        sources[[length(sources) + 1L]] <- tibble::tibble(
          stage_id = sid,
          alias = scalar_chr(src$alias),
          catalog = scalar_chr(src$catalog),
          schema = scalar_chr(src$schema),
          table = scalar_chr(src$table)
        )
      }

      # --- projections + column-level lineage --------------------------------
      for (p in stg$projections) {
        projections[[length(projections) + 1L]] <- tibble::tibble(
          stage_id = sid,
          output = scalar_chr(p$output),
          expr = scalar_chr(p$expr),
          is_aggregate = isTRUE(as.logical(p$is_aggregate)),
          has_window = isTRUE(as.logical(p$has_window)),
          has_case = isTRUE(as.logical(p$has_case)),
          # Keep the raw function names as a list column for the classifier.
          functions = list(as.character(unlist(p$functions)))
        )

        # One lineage row per source column feeding this output column. The
        # parser reports the source's *alias* in `table`; resolve it to the
        # real table name where we can.
        for (col in p$columns) {
          src_alias <- scalar_chr(col$table)
          proj_sources[[length(proj_sources) + 1L]] <- tibble::tibble(
            stage_id = sid,
            output = scalar_chr(p$output),
            src_alias = src_alias,
            src_table = resolve_alias(src_alias, alias_to_table),
            src_column = scalar_chr(col$name)
          )
        }
      }

      # --- joins + join keys -------------------------------------------------
      join_index <- 0L
      for (j in stg$joins) {
        join_index <- join_index + 1L
        joins[[length(joins) + 1L]] <- tibble::tibble(
          stage_id = sid,
          join_index = join_index,
          side = scalar_chr(j$side),
          kind = scalar_chr(j$kind),
          on = scalar_chr(j$on)
        )
        for (k in j$keys) {
          join_keys[[length(join_keys) + 1L]] <- tibble::tibble(
            stage_id = sid,
            join_index = join_index,
            left = scalar_chr(k$left),
            right = scalar_chr(k$right)
          )
        }
      }

      # --- group by ----------------------------------------------------------
      for (g in stg$group_by) {
        group_by[[length(group_by) + 1L]] <- tibble::tibble(
          stage_id = sid,
          expr = scalar_chr(g)
        )
      }
    }
  }

  structure(
    list(
      stages = bind_or_empty(stages, ir_proto$stages),
      projections = bind_or_empty(projections, ir_proto$projections),
      proj_sources = bind_or_empty(proj_sources, ir_proto$proj_sources),
      sources = bind_or_empty(sources, ir_proto$sources),
      joins = bind_or_empty(joins, ir_proto$joins),
      join_keys = bind_or_empty(join_keys, ir_proto$join_keys),
      group_by = bind_or_empty(group_by, ir_proto$group_by)
    ),
    class = "rdataflow_ir"
  )
}

# Build an alias -> table named character vector from a stage's sources.
source_alias_map <- function(srcs) {
  if (length(srcs) == 0) return(stats::setNames(character(), character()))
  aliases <- vapply(srcs, function(s) scalar_chr(s$alias), character(1))
  tables <- vapply(srcs, function(s) scalar_chr(s$table), character(1))
  stats::setNames(tables, aliases)
}

# Resolve a source alias to its table name, falling back to the alias itself
# (so an unmatched alias still produces a usable label downstream).
resolve_alias <- function(alias, alias_to_table) {
  if (is.na(alias)) return(NA_character_)
  hit <- alias_to_table[[alias]]
  if (is.null(hit)) alias else hit
}

# Coerce a possibly-NULL / possibly-list scalar from the parser into a
# length-1 character (NA when absent). reticulate maps Python None -> NULL.
scalar_chr <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  as.character(x)[[1]]
}

# Bind a list of single-row tibbles, or return a typed 0-row prototype when
# the list is empty so the IR always has the expected columns/types.
bind_or_empty <- function(rows, proto) {
  if (length(rows) == 0) proto else dplyr::bind_rows(rows)
}

# Zero-row prototypes defining each IR tibble's columns and types. Used when
# a script produces none of a given element (e.g. no joins).
ir_proto <- list(
  stages = tibble::tibble(
    stage_id = integer(), statement_index = integer(), name = character(),
    role = character(), statement_kind = character(),
    output_table = character(), is_output = logical()
  ),
  projections = tibble::tibble(
    stage_id = integer(), output = character(), expr = character(),
    is_aggregate = logical(), has_window = logical(), has_case = logical(),
    functions = list()
  ),
  proj_sources = tibble::tibble(
    stage_id = integer(), output = character(), src_alias = character(),
    src_table = character(), src_column = character()
  ),
  sources = tibble::tibble(
    stage_id = integer(), alias = character(), catalog = character(),
    schema = character(), table = character()
  ),
  joins = tibble::tibble(
    stage_id = integer(), join_index = integer(), side = character(),
    kind = character(), on = character()
  ),
  join_keys = tibble::tibble(
    stage_id = integer(), join_index = integer(), left = character(),
    right = character()
  ),
  group_by = tibble::tibble(stage_id = integer(), expr = character())
)

#' @export
print.rdataflow_ir <- function(x, ...) {
  cat(sprintf(
    "<rdataflow_ir: %d stages, %d projections, %d lineage edges, %d joins>\n",
    nrow(x$stages), nrow(x$projections), nrow(x$proj_sources), nrow(x$joins)
  ))
  invisible(x)
}

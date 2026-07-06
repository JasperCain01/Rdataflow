# ---------------------------------------------------------------------------
# Graph assembly
#
# build_graph() converts a classified rdataflow_ir into an rdataflow_graph: a
# list of tidy tibbles that describe table nodes, stage nodes, and the edges
# between them. This intermediate structure is consumed by plot_sqlflow()
# (Phase 6) which turns it into Graphviz DOT for rendering.
#
# The graph holds five tibbles:
#
#   table_nodes  - one row per physical source table (deduped across all stages)
#   stage_nodes  - one row per CTE / output stage
#   source_edges - table→stage or CTE→stage structural connections (with join
#                  type and key expressions for physical-table joins)
#   cte_edges    - CTE stage → downstream stage connections (within a statement)
#   temp_edges   - temp-table producer stage → consumer stage connections (cross-
#                  statement; mirrors cte_edges for multi-statement scripts)
#   col_edges    - column-level lineage: source-table/CTE port → stage output
#                  port, tagged with a role ("value" / "condition" / "partition"
#                  / "filter"); filter edges target the stage node (no to_port)
#
# Column sub-tibbles
#   table_nodes$columns : col_name, col_type, used, is_key
#   stage_nodes$columns : col_name, expr, transform_type
# table_nodes also carry n_hidden (count of columns suppressed by max_cols);
# stage_nodes also carry statement_index and the stage's WHERE/HAVING
# predicates, DISTINCT flag, and TOP value.
# ---------------------------------------------------------------------------

#' Build the flow graph from a lineage IR
#'
#' @param ir An `rdataflow_ir` (see [build_ir()]). Passing it through
#'   [classify_transform()] first adds transformation labels to stage nodes,
#'   but it is not required.
#' @param schema An optional `rdataflow_schema` (see [schema_from_list()] /
#'   [schema_from_con()]). When supplied table nodes list *all* catalog columns
#'   with used/key flags; without it, only columns seen in the query are shown.
#' @param show_unused_cols If `TRUE` (default), table nodes display every
#'   catalog column, with unused ones rendered in white. If `FALSE`, only
#'   columns that are projected or used as join keys are shown, producing a
#'   more compact diagram.
#' @param max_cols Maximum number of column rows to display per table node
#'   (default `Inf` = no limit). Columns that are projected or used as join
#'   keys are always kept; unused catalog columns fill the remaining space
#'   and any overflow is summarised as an "… n more columns" row. Useful
#'   against wide warehouse tables where a full catalog listing would make
#'   the node unreadably tall.
#'
#' @return An object of class `rdataflow_graph` (a named list of tibbles; see
#'   the file header for the full schema).
#' @export
build_graph <- function(ir, schema = NULL, show_unused_cols = TRUE,
                        max_cols = Inf) {
  stopifnot(inherits(ir, "rdataflow_ir"))
  if (!is.null(schema)) stopifnot(inherits(schema, "rdataflow_schema"))

  tbl_nodes  <- make_table_nodes(ir, schema, show_unused_cols, max_cols)
  stg_nodes  <- make_stage_nodes(ir)
  src_edges  <- make_source_edges(ir, tbl_nodes, stg_nodes)
  cte_edges  <- make_cte_edges(ir, stg_nodes)
  temp_edges <- make_temp_edges(ir, stg_nodes)
  col_edges  <- make_col_edges(ir, tbl_nodes, stg_nodes)

  structure(
    list(
      table_nodes  = tbl_nodes,
      stage_nodes  = stg_nodes,
      source_edges = src_edges,
      cte_edges    = cte_edges,
      temp_edges   = temp_edges,
      col_edges    = col_edges
    ),
    class = "rdataflow_graph"
  )
}

# ---------------------------------------------------------------------------
# Node builders
# ---------------------------------------------------------------------------

# Build one row per unique physical source table. CTE names and prior-stage
# output_table names (temp tables produced by earlier SELECT INTO / INSERT
# SELECT statements) are excluded because they become stage nodes, not table
# nodes. This is the cross-statement analogue of the CTE exclusion.
make_table_nodes <- function(ir, schema, show_unused_cols = TRUE,
                             max_cols = Inf) {
  # CTE names are scoped to their statement: a source ref only counts as a
  # CTE reference when the same statement defines a CTE of that name. This
  # stops a CTE in statement 1 from hiding a physical table of the same name
  # referenced in statement 2.
  is_cte <- ir$stages$role %in% c("cte", "subquery") & !is.na(ir$stages$name)
  cte_keys <- cte_scope_key(ir$stages$statement_index[is_cte],
                            ir$stages$name[is_cte])

  # Produced output_table names from any stage — these are temp tables (or
  # other produced tables) that are sources in later stages. Treating them as
  # physical table nodes would create disconnected boxes instead of edges.
  # output_name_keys() covers the '#'-stripped and leaf-name forms sqlglot
  # reports for source references. Produced names span statements by design.
  produced_names <- output_name_keys(ir$stages$output_table)

  stmt_of <- stage_stmt_lookup(ir)

  # Unique physical source tables (first occurrence wins for the catalog/schema
  # prefix; later stages referencing the same table leaf name are de-duped).
  srcs <- ir$sources
  src_stmt <- stmt_of[as.character(srcs$stage_id)]
  is_cte_ref <- !is.na(srcs$table) &
    cte_scope_key(src_stmt, srcs$table) %in% cte_keys

  phys <- srcs[!is.na(srcs$table) & !is_cte_ref &
                 !(tolower(srcs$table) %in% produced_names), , drop = FALSE] |>
    dplyr::distinct(table, .keep_all = TRUE)

  if (nrow(phys) == 0) {
    return(tibble::tibble(
      node_id = character(), label = character(), table = character(),
      n_hidden = integer(), columns = list()
    ))
  }

  purrr::map(seq_len(nrow(phys)), function(i) {
    src <- phys[i, ]
    tbl <- src$table

    # Human-readable label: schema.table when the schema is known.
    label <- paste(c(src$schema, tbl)[!is.na(c(src$schema, tbl))], collapse = ".")

    # Columns used in at least one projection referencing this table.
    used_set <- tolower(unique(
      ir$proj_sources$src_column[tolower(ir$proj_sources$src_table) == tolower(tbl)]
    ))
    used_set <- used_set[!is.na(used_set)]

    # Columns that appear as join key expressions on this table's side.
    key_set <- tolower(key_cols_for_table(ir, tbl))

    # Full column list: from schema when available; otherwise just the
    # columns the query references (used + key). Trade-off: without a schema
    # we cannot show unused columns or their types, but we avoid showing nothing.
    if (!is.null(schema)) {
      sc <- schema$columns[tolower(schema$columns$table) == tolower(tbl), ]
      col_names <- sc$column
      col_types <- sc$type
    } else {
      proj_cols <- ir$proj_sources$src_column[tolower(ir$proj_sources$src_table) == tolower(tbl)]
      col_names <- unique(c(proj_cols, key_cols_for_table(ir, tbl)))
      col_names <- col_names[!is.na(col_names) & nzchar(col_names)]
      col_types <- rep(NA_character_, length(col_names))
    }

    columns_tbl <- tibble::tibble(
      col_name = col_names,
      col_type = col_types,
      used     = tolower(col_names) %in% used_set,
      is_key   = tolower(col_names) %in% key_set
    )

    # When show_unused_cols = FALSE, drop columns that are neither projected
    # nor used as join keys — keeps the diagram compact for wide tables.
    if (!show_unused_cols) {
      columns_tbl <- columns_tbl[columns_tbl$used | columns_tbl$is_key, , drop = FALSE]
    }

    # Cap the number of displayed rows. Projected / key columns are always
    # kept; unused catalog columns fill the remaining budget in catalog
    # order. The overflow count renders as an "… n more columns" row.
    n_hidden <- 0L
    if (is.finite(max_cols) && nrow(columns_tbl) > max_cols) {
      keep <- columns_tbl$used | columns_tbl$is_key
      room <- max(0L, as.integer(max_cols) - sum(keep))
      fillers <- which(!keep)
      if (room > 0L && length(fillers) > 0L) {
        keep[fillers[seq_len(min(room, length(fillers)))]] <- TRUE
      }
      n_hidden <- sum(!keep)
      columns_tbl <- columns_tbl[keep, , drop = FALSE]
    }

    tibble::tibble(
      node_id  = graph_node_id("tbl", label),
      label    = label,
      table    = tbl,
      n_hidden = n_hidden,
      columns  = list(columns_tbl)
    )
  }) |> purrr::list_rbind()
}

# Build one row per stage (CTE or output). Each stage node carries its output
# columns with their transformation types and a human-readable label that
# summarises the stage's transformations (set by classify_transform()).
make_stage_nodes <- function(ir) {
  # Guard: no stages → return an empty tibble with the correct schema.
  # purrr::list_rbind(list()) returns NULL (not an empty tibble), which causes
  # downstream setNames() calls to fail with "attempt to set an attribute on NULL".
  if (nrow(ir$stages) == 0) {
    return(tibble::tibble(
      node_id         = character(),
      stage_id        = integer(),
      statement_index = integer(),
      name            = character(),
      role            = character(),
      output_table    = character(),
      display_name    = character(),
      transform_label = character(),
      where           = character(),
      having          = character(),
      distinct        = logical(),
      top             = character(),
      columns         = list()
    ))
  }

  has_transforms <- "stage_transforms" %in% names(ir) && !is.null(ir$stage_transforms)

  purrr::map(seq_len(nrow(ir$stages)), function(i) {
    stg <- ir$stages[i, ]
    sid <- stg$stage_id

    # Transformation summary label — only present after classify_transform().
    transform_label <- if (has_transforms) {
      tr <- ir$stage_transforms[ir$stage_transforms$stage_id == sid, ]
      if (nrow(tr) > 0) as.character(tr$label[1]) else ""
    } else {
      ""
    }

    # Output columns for this stage, with transformation category when known.
    # expr carries the full SELECT-list expression so the renderer can expose
    # exactly what each output column computes (tooltips).
    proj <- ir$projections[ir$projections$stage_id == sid, ]
    columns_tbl <- tibble::tibble(
      col_name       = proj$output,
      expr           = proj$expr,
      transform_type = if ("transform_type" %in% names(proj)) {
        proj$transform_type
      } else {
        rep("passthrough", nrow(proj))
      }
    )

    # Display name for visual labels: prefer CTE name / output table.
    display_name <- dplyr::coalesce(stg$name, stg$output_table,
                                    paste0("stage_", sid))

    tibble::tibble(
      node_id         = graph_node_id("stg", sid, stg$name),
      stage_id        = sid,
      statement_index = stg$statement_index,
      name            = stg$name,
      role            = stg$role,
      output_table    = stg$output_table,
      display_name    = display_name,
      transform_label = transform_label,
      where           = if ("where" %in% names(stg)) stg$where else NA_character_,
      having          = if ("having" %in% names(stg)) stg$having else NA_character_,
      distinct        = if ("distinct" %in% names(stg)) isTRUE(stg$distinct) else FALSE,
      top             = if ("top" %in% names(stg)) stg$top else NA_character_,
      columns         = list(columns_tbl)
    )
  }) |> purrr::list_rbind()
}

# ---------------------------------------------------------------------------
# Edge builders
# ---------------------------------------------------------------------------

# Physical-table → stage edges, one per (source_table, stage) pair. The
# first source in each stage is the FROM table (no join); subsequent sources
# correspond to the JOIN clauses in order (join_index = source_index - 1).
make_source_edges <- function(ir, tbl_nodes, stg_nodes) {
  # Statement-scoped CTE keys: only skip a source as "CTE reference" when its
  # own statement defines a CTE of that name.
  is_cte <- stg_nodes$role %in% c("cte", "subquery") & !is.na(stg_nodes$name)
  cte_keys <- cte_scope_key(stg_nodes$statement_index[is_cte],
                            stg_nodes$name[is_cte])
  stmt_of <- stage_stmt_lookup(ir)

  # as.list() is important: [[]] on a named *character* vector throws for missing
  # keys, whereas [[]] on a named *list* returns NULL, which we can test safely.
  stg_node_by_id   <- as.list(stats::setNames(stg_nodes$node_id, as.character(stg_nodes$stage_id)))
  tbl_node_by_name <- as.list(stats::setNames(tbl_nodes$node_id, tolower(tbl_nodes$table)))

  # Assign each source a position within its stage so we can look up the join.
  sources_idx <- ir$sources |>
    dplyr::group_by(.data$stage_id) |>
    dplyr::mutate(source_index = dplyr::row_number()) |>
    dplyr::ungroup()

  rows <- list()
  for (i in seq_len(nrow(sources_idx))) {
    src <- sources_idx[i, ]
    if (is.na(src$table)) next
    src_stmt <- stmt_of[[as.character(src$stage_id)]]
    if (cte_scope_key(src_stmt, src$table) %in% cte_keys) next  # CTE refs go to cte_edges

    from_node <- tbl_node_by_name[[tolower(src$table)]]
    to_node   <- stg_node_by_id[[as.character(src$stage_id)]]
    if (is.null(from_node) || is.null(to_node)) next

    # The primary FROM table (source_index 1) has no join descriptor.
    # Joined tables use join_index = source_index - 1.
    join_type <- NA_character_
    keys      <- character(0)

    if (src$source_index > 1L) {
      ji   <- src$source_index - 1L
      jrow <- ir$joins[ir$joins$stage_id == src$stage_id &
                         ir$joins$join_index == ji, ]
      if (nrow(jrow) > 0) {
        join_type <- build_join_label(jrow$side[1], jrow$kind[1])
        krows <- ir$join_keys[ir$join_keys$stage_id == src$stage_id &
                                ir$join_keys$join_index == ji, ]
        if (nrow(krows) > 0) {
          keys <- paste0(krows$left, " = ", krows$right)
        }
      }
    }

    rows[[length(rows) + 1L]] <- tibble::tibble(
      from_node_id = from_node,
      to_node_id   = to_node,
      join_type    = join_type,
      keys         = list(keys)
    )
  }

  if (length(rows) == 0) {
    return(tibble::tibble(
      from_node_id = character(), to_node_id = character(),
      join_type = character(), keys = list()
    ))
  }
  dplyr::distinct(dplyr::bind_rows(rows))
}

# CTE stage → downstream stage edges. Each source whose table name matches a
# CTE stage name *within the same statement* produces one directed edge from
# that CTE's stage node to the consuming stage node. Scoping by statement
# keeps same-named CTEs in different statements from cross-wiring.
make_cte_edges <- function(ir, stg_nodes) {
  empty <- tibble::tibble(from_node_id = character(), to_node_id = character(),
                          from_role = character())
  cte_stages <- stg_nodes[stg_nodes$role %in% c("cte", "subquery") &
                          !is.na(stg_nodes$name), ]
  if (nrow(cte_stages) == 0) {
    return(empty)
  }

  cte_node_by_key <- as.list(stats::setNames(
    cte_stages$node_id,
    cte_scope_key(cte_stages$statement_index, cte_stages$name)
  ))
  cte_role_by_key <- as.list(stats::setNames(
    cte_stages$role,
    cte_scope_key(cte_stages$statement_index, cte_stages$name)
  ))
  stg_node_by_id <- as.list(stats::setNames(stg_nodes$node_id, as.character(stg_nodes$stage_id)))
  stmt_of <- stage_stmt_lookup(ir)

  rows <- list()
  for (i in seq_len(nrow(ir$sources))) {
    src <- ir$sources[i, ]
    if (is.na(src$table)) next
    src_stmt <- stmt_of[[as.character(src$stage_id)]]
    key <- cte_scope_key(src_stmt, src$table)
    from_node <- cte_node_by_key[[key]]
    if (is.null(from_node)) next

    to_node <- stg_node_by_id[[as.character(src$stage_id)]]
    if (is.null(to_node)) next

    rows[[length(rows) + 1L]] <- tibble::tibble(
      from_node_id = from_node,
      to_node_id   = to_node,
      from_role    = cte_role_by_key[[key]]
    )
  }

  if (length(rows) == 0) {
    return(empty)
  }
  dplyr::distinct(dplyr::bind_rows(rows))
}

# Cross-statement temp-table edges: connects the output stage that produced a
# temp table to each downstream stage that reads from it. Mirrors make_cte_edges()
# but operates on output_table names (which span statement boundaries) rather
# than CTE names (which are scoped within one statement).
make_temp_edges <- function(ir, stg_nodes) {
  # Stages that have a non-empty output_table are temp-table producers.
  producer_stages <- stg_nodes[
    !is.na(stg_nodes$output_table) & nzchar(stg_nodes$output_table), ,
    drop = FALSE
  ]
  if (nrow(producer_stages) == 0L) {
    return(tibble::tibble(from_node_id = character(), to_node_id = character()))
  }

  # Map every lookup variant of the output_table name (raw, '#'-stripped,
  # leaf, both — see output_name_keys()) to the producer node_id.
  prod_node_by_tbl <- output_node_map(producer_stages)
  stg_node_by_id <- as.list(
    stats::setNames(stg_nodes$node_id, as.character(stg_nodes$stage_id))
  )

  rows <- list()
  for (i in seq_len(nrow(ir$sources))) {
    src <- ir$sources[i, ]
    if (is.na(src$table) || !nzchar(src$table)) next

    from_node <- prod_node_by_tbl[[tolower(src$table)]]
    if (is.null(from_node)) next

    to_node <- stg_node_by_id[[as.character(src$stage_id)]]
    if (is.null(to_node)) next

    # Don't self-loop: a stage that SELECT INTOs its own output_table would
    # already be in cte_edges; skip here.
    if (identical(from_node, to_node)) next

    rows[[length(rows) + 1L]] <- tibble::tibble(
      from_node_id = from_node,
      to_node_id   = to_node
    )
  }

  if (length(rows) == 0L) {
    return(tibble::tibble(from_node_id = character(), to_node_id = character()))
  }
  dplyr::distinct(dplyr::bind_rows(rows))
}

# Column-level lineage edges: one row per (source column, output column) pair
# traced through proj_sources, plus one row per WHERE-clause filter column
# traced through `filters`. The source may be a physical table node or an
# upstream CTE stage node; both are looked up by the src_table name.
#
# Every edge carries a `role`: "value" (default), "condition" (CASE WHEN
# predicate), "partition" (window PARTITION BY / ORDER BY), or "filter" (WHERE
# predicate). Filter edges target the stage node itself with `to_port = NA` —
# a WHERE-clause column narrows the stage's rows, not any one output column.
make_col_edges <- function(ir, tbl_nodes, stg_nodes) {
  col_edges_proto <- tibble::tibble(
    from_node_id = character(), from_port = character(),
    to_node_id   = character(), to_port   = character(),
    role         = character()
  )
  if (nrow(ir$proj_sources) == 0 && nrow(ir$filters) == 0) {
    return(col_edges_proto)
  }

  # CTE stage lookup — scoped to the owning statement so same-named CTEs in
  # different statements resolve to the right stage node.
  is_cte <- stg_nodes$role %in% c("cte", "subquery") & !is.na(stg_nodes$name)
  cte_node_by_key <- as.list(stats::setNames(
    stg_nodes$node_id[is_cte],
    cte_scope_key(stg_nodes$statement_index[is_cte], stg_nodes$name[is_cte])
  ))
  stmt_of <- stage_stmt_lookup(ir)

  # Temp-table output names — produced by earlier statements (cross-statement
  # by design). output_name_keys() covers '#'-stripped and leaf-name variants.
  stg_node_by_id     <- as.list(stats::setNames(stg_nodes$node_id, as.character(stg_nodes$stage_id)))
  stg_node_by_outtbl <- output_node_map(stg_nodes)
  tbl_node_by_name   <- as.list(stats::setNames(tbl_nodes$node_id, tolower(tbl_nodes$table)))

  # Resolve a source table/alias name to its upstream node_id (CTE, temp
  # producer, or physical table), scoped to the referencing stage's statement.
  resolve_source_node <- function(src_table, stage_id) {
    src_lower <- tolower(src_table)
    src_stmt  <- stmt_of[[as.character(stage_id)]]
    cte_node_by_key[[cte_scope_key(src_stmt, src_lower)]] %||%
      stg_node_by_outtbl[[src_lower]] %||%
      tbl_node_by_name[[src_lower]]
  }

  rows <- list()
  for (i in seq_len(nrow(ir$proj_sources))) {
    row <- ir$proj_sources[i, ]
    if (is.na(row$src_table) || is.na(row$src_column)) next

    to_node <- stg_node_by_id[[as.character(row$stage_id)]]
    if (is.null(to_node)) next

    from_node <- resolve_source_node(row$src_table, row$stage_id)
    if (is.null(from_node)) next

    rows[[length(rows) + 1L]] <- tibble::tibble(
      from_node_id = from_node,
      from_port    = row$src_column,
      to_node_id   = to_node,
      to_port      = row$output,
      role         = if ("role" %in% names(row) && !is.na(row$role)) row$role else "value"
    )
  }

  for (i in seq_len(nrow(ir$filters))) {
    row <- ir$filters[i, ]
    if (is.na(row$src_table) || is.na(row$src_column)) next

    to_node <- stg_node_by_id[[as.character(row$stage_id)]]
    if (is.null(to_node)) next

    from_node <- resolve_source_node(row$src_table, row$stage_id)
    if (is.null(from_node)) next

    rows[[length(rows) + 1L]] <- tibble::tibble(
      from_node_id = from_node,
      from_port    = row$src_column,
      to_node_id   = to_node,
      to_port      = NA_character_,
      role         = "filter"
    )
  }

  if (length(rows) == 0) {
    return(col_edges_proto)
  }
  dplyr::distinct(dplyr::bind_rows(rows))
}

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Build a display label for a JOIN from its side ("LEFT"/"RIGHT"/"FULL"/"")
# and kind ("INNER"/"OUTER"/"CROSS"/"APPLY"/"") components.
# Trailing "JOIN" is always appended so an empty pair still reads "JOIN" —
# except APPLY, which is its own T-SQL operator ("CROSS APPLY", not
# "APPLY JOIN").
build_join_label <- function(side, kind) {
  parts <- toupper(c(side, kind))
  parts <- parts[!is.na(parts) & nzchar(parts)]
  if ("APPLY" %in% parts) return(paste(parts, collapse = " "))
  paste(c(parts, "JOIN"), collapse = " ")
}

# Return the bare column names that appear as join key expressions on the
# given table's side, across all stages. Key expressions are stored as
# "alias.column" strings (possibly bracket-quoted); we strip brackets, split
# on the last dot, and resolve the alias through the stage's source map.
key_cols_for_table <- function(ir, tbl_name) {
  if (nrow(ir$join_keys) == 0) return(character(0))

  result <- character(0)
  for (sid in unique(ir$join_keys$stage_id)) {
    stage_srcs <- ir$sources[ir$sources$stage_id == sid, ]
    # Lower-cased alias -> lower-cased table name map for this stage.
    alias_map <- as.list(stats::setNames(tolower(stage_srcs$table), tolower(stage_srcs$alias)))

    stage_keys <- ir$join_keys[ir$join_keys$stage_id == sid, ]
    for (i in seq_len(nrow(stage_keys))) {
      for (expr in c(stage_keys$left[i], stage_keys$right[i])) {
        if (is.na(expr) || !nzchar(expr)) next

        # Strip brackets so "[c].[customer_id]" becomes "c.customer_id".
        clean <- stringr::str_remove_all(expr, "\\[|\\]")
        parts <- stringr::str_split(clean, "\\.", simplify = TRUE)[1L, ]
        if (length(parts) < 2L) next

        alias <- tolower(parts[length(parts) - 1L])
        col   <- parts[length(parts)]

        resolved <- alias_map[[alias]]
        if (!is.null(resolved) && !is.na(resolved) &&
            resolved == tolower(tbl_name)) {
          result <- c(result, col)
        }
      }
    }
  }
  unique(result)
}

# Normalise produced-table (output_table) names into every lookup key a later
# FROM reference might use. sqlglot strips '#' from #temp names, and reports
# only the leaf name for schema-qualified references, so "#t" / "dbo.summary"
# must be findable as "t" / "summary" too. Input may be a character vector;
# NA / empty entries are dropped. All keys are lower-cased.
output_name_keys <- function(x) {
  x <- tolower(x[!is.na(x) & nzchar(x)])
  if (length(x) == 0L) return(character(0))
  leaf <- sub("^.*\\.", "", x)
  unique(c(x, sub("^#", "", x), leaf, sub("^#", "", leaf)))
}

# Named-list multimap from every output_name_keys() variant of each stage's
# output_table to that stage's node_id. First registration wins on collision.
output_node_map <- function(stage_tbl) {
  key_lists <- lapply(stage_tbl$output_table, output_name_keys)
  keys <- unlist(key_lists)
  if (length(keys) == 0L) return(list())
  as.list(stats::setNames(rep(stage_tbl$node_id, lengths(key_lists)), keys))
}

# Named integer lookup stage_id (as character) -> statement_index, so edge
# builders can scope CTE-name resolution to the owning statement.
stage_stmt_lookup <- function(ir) {
  stats::setNames(ir$stages$statement_index, as.character(ir$stages$stage_id))
}

# Key used to match a source reference to a CTE: CTE names are scoped to
# their statement, so two statements may each define a CTE called "base".
# The zero-length guard matters: paste0() recycles zero-length inputs
# against the "\r" separator, which would fabricate a bogus key.
cte_scope_key <- function(statement_index, name) {
  if (length(name) == 0L) return(character(0))
  paste0(statement_index, "\r", tolower(name))
}

# Make a valid Graphviz node ID from one or more string parts. The parts are
# joined with underscores; any character not in [a-zA-Z0-9_] is replaced so
# the ID can be used unquoted in DOT output.
graph_node_id <- function(...) {
  parts <- as.character(c(...))
  parts <- parts[!is.na(parts) & nzchar(parts)]
  raw   <- paste(parts, collapse = "_")
  stringr::str_replace_all(raw, "[^a-zA-Z0-9_]", "_")
}

#' @export
print.rdataflow_graph <- function(x, ...) {
  cat(sprintf(
    paste0("<rdataflow_graph: %d table nodes, %d stage nodes, %d source edges,",
           " %d cte edges, %d temp edges, %d col edges>\n"),
    nrow(x$table_nodes), nrow(x$stage_nodes),
    nrow(x$source_edges), nrow(x$cte_edges), nrow(x$temp_edges), nrow(x$col_edges)
  ))
  invisible(x)
}

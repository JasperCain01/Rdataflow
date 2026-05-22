# ---------------------------------------------------------------------------
# Graph assembly
#
# build_graph() converts a classified rdataflow_ir into an rdataflow_graph: a
# list of tidy tibbles that describe table nodes, stage nodes, and the edges
# between them. This intermediate structure is consumed by plot_sqlflow()
# (Phase 6) which turns it into Graphviz DOT for rendering.
#
# The graph holds four tibbles:
#
#   table_nodes  - one row per physical source table (deduped across all stages)
#   stage_nodes  - one row per CTE / output stage
#   source_edges - table→stage or CTE→stage structural connections (with join
#                  type and key expressions for physical-table joins)
#   cte_edges    - CTE stage → downstream stage connections
#   col_edges    - column-level lineage: source-table/CTE port → stage output port
#
# Column sub-tibbles
#   table_nodes$columns : col_name, col_type, used, is_key
#   stage_nodes$columns : col_name, transform_type
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
#'
#' @return An object of class `rdataflow_graph` (a named list of tibbles; see
#'   the file header for the full schema).
#' @export
build_graph <- function(ir, schema = NULL, show_unused_cols = TRUE) {
  stopifnot(inherits(ir, "rdataflow_ir"))
  if (!is.null(schema)) stopifnot(inherits(schema, "rdataflow_schema"))

  tbl_nodes  <- make_table_nodes(ir, schema, show_unused_cols)
  stg_nodes  <- make_stage_nodes(ir)
  src_edges  <- make_source_edges(ir, tbl_nodes, stg_nodes)
  cte_edges  <- make_cte_edges(ir, stg_nodes)
  col_edges  <- make_col_edges(ir, tbl_nodes, stg_nodes)

  structure(
    list(
      table_nodes  = tbl_nodes,
      stage_nodes  = stg_nodes,
      source_edges = src_edges,
      cte_edges    = cte_edges,
      col_edges    = col_edges
    ),
    class = "rdataflow_graph"
  )
}

# ---------------------------------------------------------------------------
# Node builders
# ---------------------------------------------------------------------------

# Build one row per unique physical source table. CTE names are excluded
# because they become stage nodes, not table nodes.
make_table_nodes <- function(ir, schema, show_unused_cols = TRUE) {
  # Lower-cased CTE names for quick membership testing.
  cte_names <- tolower(ir$stages$name[ir$stages$role == "cte" & !is.na(ir$stages$name)])

  # Unique physical source tables (first occurrence wins for the catalog/schema
  # prefix; later stages referencing the same table leaf name are de-duped).
  phys <- ir$sources |>
    dplyr::filter(!is.na(table), !tolower(table) %in% cte_names) |>
    dplyr::distinct(table, .keep_all = TRUE)

  if (nrow(phys) == 0) {
    return(tibble::tibble(
      node_id = character(), label = character(), table = character(),
      columns = list()
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

    tibble::tibble(
      node_id = graph_node_id("tbl", label),
      label   = label,
      table   = tbl,
      columns = list(columns_tbl)
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
      name            = character(),
      role            = character(),
      output_table    = character(),
      display_name    = character(),
      transform_label = character(),
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
    proj <- ir$projections[ir$projections$stage_id == sid, ]
    columns_tbl <- tibble::tibble(
      col_name       = proj$output,
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
      name            = stg$name,
      role            = stg$role,
      output_table    = stg$output_table,
      display_name    = display_name,
      transform_label = transform_label,
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
  cte_names        <- tolower(stg_nodes$name[stg_nodes$role == "cte" & !is.na(stg_nodes$name)])
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
    if (tolower(src$table) %in% cte_names) next  # CTE refs go to cte_edges

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
# CTE stage name produces one directed edge from that CTE's stage node to the
# consuming stage node.
make_cte_edges <- function(ir, stg_nodes) {
  cte_stages <- stg_nodes[stg_nodes$role == "cte" & !is.na(stg_nodes$name), ]
  if (nrow(cte_stages) == 0) {
    return(tibble::tibble(from_node_id = character(), to_node_id = character()))
  }

  cte_node_by_name <- as.list(stats::setNames(cte_stages$node_id, tolower(cte_stages$name)))
  stg_node_by_id   <- as.list(stats::setNames(stg_nodes$node_id, as.character(stg_nodes$stage_id)))

  rows <- list()
  for (i in seq_len(nrow(ir$sources))) {
    src <- ir$sources[i, ]
    if (is.na(src$table)) next
    from_node <- cte_node_by_name[[tolower(src$table)]]
    if (is.null(from_node)) next

    to_node <- stg_node_by_id[[as.character(src$stage_id)]]
    if (is.null(to_node)) next

    rows[[length(rows) + 1L]] <- tibble::tibble(
      from_node_id = from_node,
      to_node_id   = to_node
    )
  }

  if (length(rows) == 0) {
    return(tibble::tibble(from_node_id = character(), to_node_id = character()))
  }
  dplyr::distinct(dplyr::bind_rows(rows))
}

# Column-level lineage edges: one row per (source column, output column) pair
# traced through proj_sources. The source may be a physical table node or an
# upstream CTE stage node; both are looked up by the src_table name.
make_col_edges <- function(ir, tbl_nodes, stg_nodes) {
  if (nrow(ir$proj_sources) == 0) {
    return(tibble::tibble(
      from_node_id = character(), from_port = character(),
      to_node_id   = character(), to_port   = character()
    ))
  }

  cte_names        <- tolower(stg_nodes$name[stg_nodes$role == "cte" & !is.na(stg_nodes$name)])
  stg_node_by_id   <- as.list(stats::setNames(stg_nodes$node_id, as.character(stg_nodes$stage_id)))
  stg_node_by_name <- as.list(stats::setNames(stg_nodes$node_id, tolower(stg_nodes$name)))
  tbl_node_by_name <- as.list(stats::setNames(tbl_nodes$node_id, tolower(tbl_nodes$table)))

  rows <- list()
  for (i in seq_len(nrow(ir$proj_sources))) {
    row <- ir$proj_sources[i, ]
    if (is.na(row$src_table) || is.na(row$src_column)) next

    to_node <- stg_node_by_id[[as.character(row$stage_id)]]
    if (is.null(to_node)) next

    src_lower <- tolower(row$src_table)
    from_node <- if (src_lower %in% cte_names) {
      stg_node_by_name[[src_lower]]
    } else {
      tbl_node_by_name[[src_lower]]
    }
    if (is.null(from_node)) next

    rows[[length(rows) + 1L]] <- tibble::tibble(
      from_node_id = from_node,
      from_port    = row$src_column,
      to_node_id   = to_node,
      to_port      = row$output
    )
  }

  if (length(rows) == 0) {
    return(tibble::tibble(
      from_node_id = character(), from_port = character(),
      to_node_id   = character(), to_port   = character()
    ))
  }
  dplyr::distinct(dplyr::bind_rows(rows))
}

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Build a display label for a JOIN from its side ("LEFT"/"RIGHT"/"FULL"/"")
# and kind ("INNER"/"OUTER"/"CROSS"/"") components.
# Trailing "JOIN" is always appended so an empty pair still reads "JOIN".
build_join_label <- function(side, kind) {
  parts <- toupper(c(side, kind))
  parts <- parts[!is.na(parts) & nzchar(parts)]
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
    "<rdataflow_graph: %d table nodes, %d stage nodes, %d source edges, %d cte edges, %d col edges>\n",
    nrow(x$table_nodes), nrow(x$stage_nodes),
    nrow(x$source_edges), nrow(x$cte_edges), nrow(x$col_edges)
  ))
  invisible(x)
}

# ---------------------------------------------------------------------------
# Textual narrative
#
# explain_sqlflow() renders the lineage IR as a plain-text or markdown
# narrative: one section per statement, one block per stage, describing what
# each stage reads, how tables are joined (and on which keys), how rows are
# grouped and filtered, and what every output column computes. It is the
# textual twin of plot_sqlflow() — useful for PR descriptions, code review,
# documentation, and as an accessible alternative to the diagram.
# ---------------------------------------------------------------------------

#' Explain a SQL script's data flow as text
#'
#' Produces a human-readable narrative of the lineage model: per statement
#' and stage, what is read, joined (with keys), grouped, filtered, and
#' computed. Accepts either a SQL string (the full parse pipeline is run) or
#' an [build_ir()] result, classified or not.
#'
#' @param x A SQL script as a length-1 character string, or an
#'   `rdataflow_ir` object.
#' @param schema,dialect Passed to [parse_sql()] when `x` is a SQL string;
#'   ignored otherwise.
#' @param format `"text"` (default) for indented plain text, or
#'   `"markdown"` for a bulleted markdown document.
#'
#' @return A character vector of lines, classed `rdataflow_explanation` so
#'   that printing renders it verbatim. Use `cat(..., sep = "\n")` or
#'   `writeLines()` to embed it elsewhere.
#' @export
#'
#' @examples
#' \dontrun{
#' explain_sqlflow("SELECT customer_id, SUM(amount) AS total
#'                  FROM dbo.orders GROUP BY customer_id")
#' }
explain_sqlflow <- function(x, schema = NULL, dialect = "tsql",
                            format = c("text", "markdown")) {
  format <- match.arg(format)

  ir <- if (inherits(x, "rdataflow_ir")) {
    x
  } else if (is.character(x)) {
    sql <- paste(x, collapse = "\n")
    parsed <- parse_sql(sql, schema = schema, dialect = dialect)
    # Same no-silent-gaps contract as sql_dataflow(): a narrative missing a
    # statement must say so, not just omit it.
    notify_skipped(parsed$skipped)
    build_ir(parsed)
  } else {
    rlang::abort("`x` must be a SQL string or an rdataflow_ir object.")
  }
  # Transformation categories make the narrative sharper; add them when the
  # caller hasn't already.
  if (!"transform_type" %in% names(ir$projections)) {
    ir <- classify_transform(ir)
  }

  md <- identical(format, "markdown")
  lines <- character(0)

  for (stmt_idx in unique(ir$stages$statement_index)) {
    stmt_stages <- ir$stages[ir$stages$statement_index == stmt_idx, , drop = FALSE]
    kind <- stmt_stages$statement_kind[1]
    out_tbl <- stmt_stages$output_table[1]

    header <- sprintf(
      "Statement %d (%s%s)", stmt_idx, kind,
      if (!is.na(out_tbl) && nzchar(out_tbl)) paste0(" -> ", out_tbl) else ""
    )
    lines <- c(lines, if (md) paste0("## ", header) else header)

    for (i in seq_len(nrow(stmt_stages))) {
      stg <- stmt_stages[i, ]
      lines <- c(lines, explain_stage(ir, stg, md))
    }
    lines <- c(lines, "")
  }

  structure(lines, class = c("rdataflow_explanation", "character"))
}

#' @export
print.rdataflow_explanation <- function(x, ...) {
  cat(x, sep = "\n")
  invisible(x)
}

# Narrate one stage: its reads/joins, grouping, filter, and output columns.
# Returns a character vector of lines. `md` switches markdown bullets on.
explain_stage <- function(ir, stg, md = FALSE) {
  sid <- stg$stage_id
  b1 <- if (md) "- " else "  "        # stage-level indent
  b2 <- if (md) "  - " else "    - "  # detail-level indent

  stage_title <- if (identical(stg$role, "cte")) {
    sprintf("Stage '%s' (CTE):", stg$name)
  } else if (identical(stg$role, "subquery")) {
    sprintf("Stage '%s' (derived table):", stg$name)
  } else if (!is.na(stg$output_table) && nzchar(stg$output_table)) {
    sprintf("Output stage -> %s:", stg$output_table)
  } else {
    "Output stage (result set):"
  }

  lines <- paste0(b1, if (md) paste0("**", sub(":$", "", stage_title), "**")
                  else stage_title)

  # --- reads / joins ---------------------------------------------------------
  srcs <- ir$sources[ir$sources$stage_id == sid, , drop = FALSE]
  if (nrow(srcs) > 0) {
    first <- srcs[1, ]
    lines <- c(lines, paste0(b2, "reads ", source_label(first)))

    if (nrow(srcs) > 1) {
      for (j in 2:nrow(srcs)) {
        src <- srcs[j, ]
        ji <- j - 1L
        jrow <- ir$joins[ir$joins$stage_id == sid & ir$joins$join_index == ji, ]
        krows <- ir$join_keys[ir$join_keys$stage_id == sid &
                                ir$join_keys$join_index == ji, ]
        jlabel <- if (nrow(jrow) > 0) {
          build_join_label(jrow$side[1], jrow$kind[1])
        } else {
          "JOIN"
        }
        keys_txt <- if (nrow(krows) > 0) {
          paste0(" ON ", paste(paste0(krows$left, " = ", krows$right),
                               collapse = " AND "))
        } else {
          ""
        }
        verb <- sub("applys$", "applies", paste0(tolower(jlabel), "s"))
        lines <- c(lines, paste0(
          b2, verb, " ", source_label(src), keys_txt
        ))
      }
    }
  }

  # --- grouping --------------------------------------------------------------
  grp <- ir$group_by[ir$group_by$stage_id == sid, , drop = FALSE]
  if (nrow(grp) > 0) {
    cols <- unique(vapply(grp$expr, clean_identifier, character(1)))
    lines <- c(lines, paste0(b2, "groups by ", paste(cols, collapse = ", ")))
  }

  # --- filter ----------------------------------------------------------------
  if ("where" %in% names(stg) && !is.na(stg$where) && nzchar(stg$where)) {
    lines <- c(lines, paste0(b2, "filters: WHERE ", stg$where))
  }

  # --- DISTINCT / TOP / HAVING ------------------------------------------------
  if ("distinct" %in% names(stg) && isTRUE(stg$distinct)) {
    lines <- c(lines, paste0(b2, "keeps distinct rows"))
  }
  if ("top" %in% names(stg) && !is.na(stg$top) && nzchar(stg$top)) {
    lines <- c(lines, paste0(b2, "keeps top ", stg$top))
  }
  if ("having" %in% names(stg) && !is.na(stg$having) && nzchar(stg$having)) {
    lines <- c(lines, paste0(b2, "filters groups: HAVING ", stg$having))
  }

  # --- output columns --------------------------------------------------------
  proj <- ir$projections[ir$projections$stage_id == sid, , drop = FALSE]
  if (nrow(proj) > 0) {
    is_pass <- proj$transform_type == "passthrough"

    if (any(!is_pass)) {
      lines <- c(lines, paste0(b2, "computes:"))
      b3 <- if (md) "    - " else "      * "
      for (k in which(!is_pass)) {
        expr_body <- strip_alias(proj$expr[k])
        lines <- c(lines, sprintf(
          "%s%s = %s  [%s]", b3, proj$output[k], expr_body,
          proj$transform_type[k]
        ))
      }
    }
    if (any(is_pass)) {
      lines <- c(lines, paste0(
        b2, "passes through ",
        paste(proj$output[is_pass], collapse = ", ")
      ))
    }
  }

  lines
}

# "schema.table" (or bare table) with the alias when it differs.
source_label <- function(src) {
  name <- paste(c(src$schema, src$table)[!is.na(c(src$schema, src$table))],
                collapse = ".")
  if (!is.na(src$alias) && nzchar(src$alias) &&
      tolower(src$alias) != tolower(src$table)) {
    paste0(name, " (as ", src$alias, ")")
  } else {
    name
  }
}

# Drop a trailing "AS alias" from a projection expression for display,
# since the alias is already shown on the left of the equals sign.
strip_alias <- function(expr) {
  stringr::str_remove(expr, "(?i)\\s+AS\\s+\\[?[A-Za-z0-9_ ]+\\]?\\s*$")
}

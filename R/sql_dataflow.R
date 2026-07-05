# ---------------------------------------------------------------------------
# Top-level API
#
# sql_dataflow() is the single entry-point for users: give it a SQL script
# and an optional schema, and it runs the full pipeline — parse → IR →
# classify transformations → assemble graph → render — returning a
# DiagrammeR widget ready to display in the RStudio Viewer or an R Markdown
# document.
#
# The lower-level functions (parse_sql, build_ir, classify_transform,
# build_graph, plot_sqlflow) are all exported so power users can inspect or
# customise each stage independently.
# ---------------------------------------------------------------------------

#' Read a SQL script from a file
#'
#' A convenience wrapper around [readLines()] that (a) detects the file's
#' encoding from its byte-order mark — SQL Server Management Studio saves
#' `.sql` files as UTF-16 LE with a BOM by default, which plain `readLines()`
#' silently mangles — (b) suppresses the `"incomplete final line"` warning
#' produced when a SQL file does not end with a newline (common when the last
#' character is a `;`), and (c) collapses the result into a single string
#' ready for [sql_dataflow()].
#'
#' Encoding detection: a UTF-16 LE/BE or UTF-8 BOM is honoured; without a
#' BOM, a NUL byte early in the file is taken as BOM-less UTF-16 LE;
#' otherwise the file is read as UTF-8.
#'
#' @param path Path to a `.sql` file.
#' @return A length-1 character string containing the full SQL script.
#' @seealso [sql_dataflow()]
#' @export
#'
#' @examples
#' \dontrun{
#' sql <- read_sql("my_script.sql")
#' sql_dataflow(sql)
#' }
read_sql <- function(path) {
  head_bytes <- readBin(path, "raw", n = 64L)

  encoding <- if (length(head_bytes) >= 2L &&
                  head_bytes[1] == as.raw(0xFF) && head_bytes[2] == as.raw(0xFE)) {
    "UTF-16LE"
  } else if (length(head_bytes) >= 2L &&
             head_bytes[1] == as.raw(0xFE) && head_bytes[2] == as.raw(0xFF)) {
    "UTF-16BE"
  } else if (any(head_bytes == as.raw(0x00))) {
    # No BOM but NUL bytes present: almost certainly BOM-less UTF-16. Even
    # positions NUL => big-endian ASCII layout; otherwise assume LE.
    if (length(head_bytes) >= 1L && head_bytes[1] == as.raw(0x00)) "UTF-16BE"
    else "UTF-16LE"
  } else {
    "UTF-8"   # also covers the UTF-8 BOM, stripped below
  }

  con <- file(path, encoding = encoding)
  on.exit(close(con))
  txt <- paste(readLines(con, warn = FALSE), collapse = "\n")

  # Strip a leading BOM character if the reader preserved it.
  sub("^\uFEFF", "", txt)
}

#' Visualise the column-level data flow of a SQL script
#'
#' Parses a SQL script using `sqlglot` (via `reticulate`), builds a
#' column-level lineage model, and renders it as a Graphviz flow diagram.
#' Physical source tables are shown on the left with all catalog columns
#' highlighted by role (projected / join key); CTE and output stages are shown
#' on the right with columns colour-coded by transformation type.
#'
#' @param sql A SQL script as a length-1 character string. Multiple statements
#'   are supported; each is processed independently.
#' @param schema An optional `rdataflow_schema` (see [schema_from_list()] or
#'   [schema_from_con()]). When supplied, `SELECT *` is expanded and
#'   unqualified column references are resolved against the catalog. Table
#'   nodes will show all catalog columns, not just the ones referenced in the
#'   query.
#' @param dialect `sqlglot` dialect name passed to the parser. Defaults to
#'   `"tsql"` (SQL Server / T-SQL). Other valid values include `"spark"`,
#'   `"bigquery"`, `"postgres"`, `"mysql"`, etc.
#' @param show_col_edges If `TRUE` (default), edges connect individual source
#'   columns to the output columns they feed (port-to-port). If `FALSE`, only
#'   structural table→stage edges are drawn, labelled with join type.
#' @param show_unused_cols If `TRUE` (default), table nodes display every
#'   catalog column, with unused ones rendered in white. If `FALSE`, only
#'   columns that are projected or used as join keys are shown, producing a
#'   more compact diagram. Useful when the schema has many columns and the
#'   diagram becomes too tall to view comfortably.
#' @param show_legend If `TRUE` (default), a colour-coding legend is appended
#'   to the diagram, explaining node header colours, column role colours,
#'   transformation type colours, and edge styles.
#' @param rank_lanes If `TRUE` (default), nodes at the same dependency depth
#'   are aligned in the same column using `rank=same` constraints. This turns
#'   parallel branches into aligned vertical lanes. Pass `FALSE` to let
#'   Graphviz place nodes freely.
#'
#' @return A `DiagrammeR` / htmlwidget object. Displays automatically in the
#'   RStudio Viewer, R Markdown, and Shiny. Call [graph_to_dot()] on the
#'   intermediate [build_graph()] result to obtain the raw DOT string.
#'
#' @seealso [schema_from_list()], [schema_from_con()], [build_graph()],
#'   [plot_sqlflow()], [graph_to_dot()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Minimal usage (no schema — only referenced columns are shown)
#' sql_dataflow(
#'   "SELECT o.customer_id, SUM(o.amount) AS total
#'    FROM dbo.orders o
#'    GROUP BY o.customer_id"
#' )
#'
#' # With an offline schema so all catalog columns appear
#' s <- schema_from_list(list(
#'   "dbo.orders" = c(order_id = "INT", customer_id = "INT", amount = "DECIMAL")
#' ))
#' sql_dataflow(
#'   "SELECT customer_id, SUM(amount) AS total
#'    FROM dbo.orders
#'    GROUP BY customer_id",
#'   schema = s
#' )
#'
#' # Compact view: hide catalog columns not referenced in the query
#' sql_dataflow(
#'   "SELECT customer_id, SUM(amount) AS total
#'    FROM dbo.orders
#'    GROUP BY customer_id",
#'   schema = s,
#'   show_unused_cols = FALSE
#' )
#'
#' # With a live database connection (SQL Server via odbc)
#' con <- DBI::dbConnect(odbc::odbc(), dsn = "my_dsn")
#' s   <- schema_from_con(con)
#' sql_dataflow(read_sql("my_script.sql"), schema = s)
#' DBI::dbDisconnect(con)
#' }
sql_dataflow <- function(sql, schema = NULL, dialect = "tsql",
                         show_col_edges = TRUE, show_unused_cols = TRUE,
                         show_legend = TRUE, rank_lanes = TRUE) {
  stopifnot(is.character(sql), length(sql) >= 1L)

  # Collapse multi-element vectors (e.g. from readLines()) into one string.
  if (length(sql) > 1L) sql <- paste(sql, collapse = "\n")

  # Full pipeline: parse → IR → classify → graph → render.
  parsed     <- parse_sql(sql, schema = schema, dialect = dialect)
  notify_skipped(parsed$skipped)
  ir         <- build_ir(parsed)
  classified <- classify_transform(ir)
  graph      <- build_graph(classified, schema = schema,
                            show_unused_cols = show_unused_cols)
  plot_sqlflow(
    graph,
    show_col_edges = show_col_edges,
    show_legend    = show_legend,
    rank_lanes     = rank_lanes
  )
}

# Surface the parse-stage skip log to the user. A lineage diagram that
# silently omits statements is actively misleading, so anything that could
# mean missing lineage (unrecognised statements, parse/qualify failures,
# unresolved variables) is raised as a warning listing each entry. Benign
# skips — statements that carry no lineage by design (DROP, CREATE INDEX,
# INSERT ... VALUES) — are summarised in a message instead.
notify_skipped <- function(skipped) {
  if (length(skipped) == 0L) return(invisible(NULL))

  benign <- grepl("skipped non-SELECT statement", skipped, fixed = TRUE)

  if (any(!benign)) {
    rlang::warn(paste0(
      "Some statements could not be fully processed; ",
      "the diagram may be missing lineage:\n",
      paste0("  - ", skipped[!benign], collapse = "\n")
    ))
  }
  if (any(benign)) {
    rlang::inform(sprintf(
      paste0("%d statement(s) with no lineage contribution ",
             "(DROP / CREATE INDEX / INSERT ... VALUES) skipped."),
      sum(benign)
    ))
  }
  invisible(NULL)
}

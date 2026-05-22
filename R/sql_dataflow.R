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
#' A convenience wrapper around [readLines()] that suppresses the
#' `"incomplete final line"` warning produced when a SQL file does not end
#' with a newline (common when the last character is a `;`), and collapses
#' the result into a single string ready for [sql_dataflow()].
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
  paste(readLines(path, warn = FALSE), collapse = "\n")
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
#' # With a live database connection (SQL Server via odbc)
#' con <- DBI::dbConnect(odbc::odbc(), dsn = "my_dsn")
#' s   <- schema_from_con(con)
#' sql_dataflow(read_sql("my_script.sql"), schema = s)
#' DBI::dbDisconnect(con)
#' }
sql_dataflow <- function(sql, schema = NULL, dialect = "tsql",
                         show_col_edges = TRUE) {
  stopifnot(is.character(sql), length(sql) >= 1L)

  # Collapse multi-element vectors (e.g. from readLines()) into one string.
  if (length(sql) > 1L) sql <- paste(sql, collapse = "\n")

  # Full pipeline: parse → IR → classify → graph → render.
  parsed     <- parse_sql(sql, schema = schema, dialect = dialect)
  ir         <- build_ir(parsed)
  classified <- classify_transform(ir)
  graph      <- build_graph(classified, schema = schema)
  plot_sqlflow(graph, show_col_edges = show_col_edges)
}

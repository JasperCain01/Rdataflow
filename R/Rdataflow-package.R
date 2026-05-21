#' Rdataflow: Visualize Column-Level Data Flow of SQL Scripts
#'
#' Rdataflow parses a SQL script (T-SQL / SQL Server first) and, using the
#' column metadata of the referenced tables, builds a column-level data
#' lineage model and renders it as a flow diagram.
#'
#' The high-level entry point is [sql_dataflow()]. The pipeline is built from
#' independently testable layers: schema resolution ([schema_from_list()] /
#' [schema_from_con()]), SQL parsing ([parse_sql()]), lineage IR construction
#' ([build_ir()]), transformation classification, graph assembly, and
#' rendering.
#'
#' @keywords internal
#' @importFrom rlang .data
"_PACKAGE"

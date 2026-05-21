# Rdataflow

## Purpose

Rdataflow is an R package that visualizes the **column-level data flow of a SQL
script**. Given a SQL script and the column metadata of the tables it
references, it reverse-engineers:

- which columns each query/CTE/stage selects (highlighting the used columns
  against the full set of available columns),
- how tables are joined and on which keys (join keys are highlighted), and
- the key transformations applied at each stage (aggregations, date-diffs,
  counts, window functions, CASE expressions, casts, ...), shown as boxes on
  the flow lines between stages.

The output is a flow diagram in the style of column-level lineage tools
(e.g. SQLFlow): table boxes listing every column, with edges drawn between the
specific columns that flow from one stage to the next.

## Architecture (build order)

The package is a pipeline of independently testable layers:

1. **Schema/metadata** (`schema_*.R`) — a normalized schema object mapping
   `table -> {column, type, ordinal}`. Built either from a live database via
   `DBI`/`odbc` (`schema_from_con()`) or from a plain named list
   (`schema_from_list()`, the testing seam — CI never needs a live DB).
2. **Parser adapter** (`parse_sql.R`) — wraps Python `sqlglot` via `reticulate`
   to parse and qualify SQL into an AST.
3. **Lineage IR** (`build_ir.R`) — a per-stage intermediate representation:
   inputs, projections (with source columns + raw expr), joins, filters,
   group-bys.
4. **Transformation classifier** (`classify_transform.R`) — tags each
   projection/stage with its transformation type.
5. **Graph assembly** (`build_graph.R`) — nodes/edges with column-level detail.
6. **Renderer** (`plot_sqlflow.R`) — Graphviz HTML-label nodes via `DiagrammeR`.
7. **Top-level API** (`sql_dataflow.R`).

## Key technical decisions

- **SQL dialect:** T-SQL / SQL Server first (`sqlglot` dialect `"tsql"`).
  Handle `[bracket]` quoting, `#temp` tables, table variables, `SELECT INTO`,
  `MERGE`, `CROSS/OUTER APPLY`, `PIVOT/UNPIVOT`, CTEs.
- **Parser:** Python `sqlglot` via `reticulate` (dialect-aware, has built-in
  column lineage + a schema-aware qualifier for `*` expansion).
- **Metadata:** primary source is a live `DBI`/`odbc` connection; the same
  normalized schema object can be built from a list for offline testing.
- **Renderer:** `DiagrammeR` / Graphviz HTML-like labels with column ports.
- Database connections are created by the user through the **`odbc`** package.

## Design guidance

- Use tidyverse packages and syntax wherever possible (`dplyr`, `purrr`,
  `stringr`, `tibble`, native pipe).
- Follow the style and principles of *R for Data Science*
  (https://r4ds.hadley.nz/).
- All code chunks should be commented explaining what they do and aim to
  achieve.

## Git workflow

- Work on branches prefixed with `claude_` (e.g. `claude_sql-dataflow`).
- Make **small, frequent commits** as work progresses — do not batch large
  changes into one commit.
- Individual commits do **not** require user approval.
- **Always ask** before merging a `claude_` branch into `main`.
- Delete `claude_` branches after they are merged.
- The previous R-script lineage implementation is preserved on the
  `legacy-r-script-dataflow` branch (a possible future add-on); do not delete
  it.

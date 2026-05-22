# Rdataflow (development version)

## New features

* `classify_statements()` classifies each SQL statement in a split script by
  kind (`"declare"`, `"create_table"`, `"insert_select"`, `"select_into"`,
  `"select"`, etc.).

* `extract_declare()` and `substitute_vars()` parse `DECLARE`/`SET` statements
  and substitute `@variable` references with their literal values or typed
  sentinels before SQL reaches the Python tokeniser. This is the primary
  crash-avoidance mechanism for production T-SQL scripts.

* `merge_temp_schema()`, `new_temp_registry()`, `register_temp_table()` — a
  temp-table schema registry that tracks `#temp` column metadata across
  statements so the sqlglot qualifier can resolve temp-table columns.

* `parse_one_select_isolated()` runs each SELECT-bearing statement inside a
  persistent `callr::r_session` child process. A C-level crash or segfault in
  sqlglot is contained to the child; the parent R session always survives.

* `parse_sql()` now handles procedural T-SQL scripts. `DECLARE`, `SET`,
  `CREATE TABLE`, `DROP TABLE`, `INSERT … VALUES`, and `CREATE INDEX` are
  handled natively in R. Only SELECT-bearing statements are sent to
  Python/sqlglot, each in a subprocess. The return value gains a `$skipped`
  element listing any statements that could not be parsed.

* `split_statements()` splits a T-SQL script into individual statements using a
  character-level state machine. Recognises `;` and lone `GO` as terminators
  only in normal state — string literals, block/line comments, and
  bracket-quoted identifiers are opaque to the boundary detector.

* `build_graph()` gains cross-statement temp-table edges (`$temp_edges`):
  stage nodes that produce `#temp` tables are connected to the stages that
  consume them, rendering the full temp-table chain in the flow diagram.

* `graph_to_dot()` / `plot_sqlflow()` / `sql_dataflow()` gain a `show_legend`
  argument (default `TRUE`). When enabled, a colour-coding legend cluster is
  appended to the diagram, explaining node header colours, column role colours,
  transformation type colours, and edge styles.

* `graph_to_dot()` / `plot_sqlflow()` / `sql_dataflow()` gain a `rank_lanes`
  argument (default `TRUE`). When enabled, `rank=same` constraints align all
  nodes at the same dependency depth into the same column, turning parallel
  branches into aligned vertical lanes and making complex multi-stage scripts
  much easier to follow.

* `sql_dataflow()` / `build_graph()` gain a `show_unused_cols` argument. When
  `FALSE`, table nodes display only projected and join-key columns, producing a
  more compact diagram for wide tables.

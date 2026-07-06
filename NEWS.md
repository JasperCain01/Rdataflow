# Rdataflow (development version)

## Package-review improvements

### New features

* `explain_sqlflow()` renders the lineage model as a plain-text or markdown
  narrative — per statement and stage: what is read, joined (with keys),
  grouped, filtered, and what every output column computes. Useful for PR
  descriptions, code review, and as an accessible alternative to the diagram.

* `save_sqlflow()` exports a diagram to `.dot`/`.gv` (no extra dependencies)
  or `.svg`/`.png`/`.pdf` (via `DiagrammeRsvg` + `rsvg`, now in Suggests).

* Diagrams now show much more of what the pipeline already knows:
  - stage columns carry hover tooltips with the full SQL expression they
    compute; the stage's `WHERE` predicate renders as a footer row (full
    text on hover);
  - join edges are labelled with their key equalities
    (`LEFT JOIN` + `r.region_id = c.region_id`), also shown on hover;
  - the default column-lineage view draws the faint structural skeleton
    underneath, so join types stay visible and filter-only tables no
    longer float disconnected;
  - the legend is dynamic — only categories present in the diagram are
    listed.

* Multi-statement scripts draw each statement's stages inside a labelled
  cluster ("Statement 2 -> #summary"); disable with
  `cluster_statements = FALSE`.

* `sql_dataflow()` surfaces the parse skip log: possible lineage losses
  (unrecognised statements, parse/qualify failures, unresolved `@vars`)
  raise a warning listing each entry; benign skips are summarised in a
  message. The Python layer now reports every failure reason instead of
  swallowing exceptions.

* Derived-table subqueries (`FROM/JOIN (SELECT ...) alias`) and
  `CROSS/OUTER APPLY` targets become their own stages, so their inner
  tables and column lineage are attributed correctly. `UNION` /
  `EXCEPT` / `INTERSECT` extract every branch instead of silently
  keeping only the first. `INSERT INTO t (c1, c2) SELECT ...` names
  output columns from the INSERT list.

* Control-flow wrappers (`IF`, `WHILE`, `BEGIN ... END`, `BEGIN TRY`)
  are unwrapped so the statements they govern keep their lineage;
  `BEGIN TRAN`/`COMMIT`/`ROLLBACK` classify as benign transactions.

* New display options: `max_cols` truncates wide table nodes with an
  "… n more columns" row (projected/key columns always kept);
  `rankdir = "TB"` for top-to-bottom layout.

* `read_sql()` detects UTF-16 LE/BE byte-order marks (the SSMS default
  save format), BOM-less UTF-16, and strips UTF-8 BOMs.

* An introductory vignette (`vignette("rdataflow")`) walks through
  `schema_from_list()` -> `sql_dataflow()` -> `explain_sqlflow()` ->
  the structural overview mode -> `max_cols` -> multi-statement temp-table
  chaining, with pre-generated example diagrams.

* Column lineage now distinguishes *how* a source column is used, not just
  that it is: columns in a `CASE WHEN` predicate are tagged `"condition"`,
  columns in a window's `PARTITION BY` / `ORDER BY` are tagged
  `"partition"`, and everything else is `"value"`. Diagrams render
  condition/partition edges dotted in a lighter blue, with the role named
  on hover.

* `WHERE`-clause columns are now traced as lineage edges (role `"filter"`):
  a dotted grey edge from the source column to the stage it filters
  (targeting the stage itself, since a filter narrows rows rather than
  feeding one output column). Previously a column used only in a `WHERE`
  predicate contributed no edge at all.

### Bug fixes

* `GO` is only treated as a batch terminator when it is the first token on
  its line, matching SQL Server's rule — `SELECT 1 AS go` no longer splits.
  `GO 5` repeat counts are consumed with the terminator.

* Nested block comments (`/* a /* b */ c */`), which T-SQL allows, are now
  handled by the statement splitter and the classifier's comment stripper;
  comment markers inside string literals are ignored.

* Multi-variable `DECLARE @a INT = 5, @b INT = 6` registers every variable
  (previously none were registered).

* `CREATE TABLE dbo.index_stats (...)` is no longer misclassified as
  `CREATE INDEX`; `SELECT ... INTO` detection is depth-0, string- and
  bracket-aware, so `'went into town'` or `[into]` can't trigger it.

* Bracket-quoted and schema-qualified output tables
  (`INTO [dbo].[summary]`) are extracted correctly, and produced-table
  names are matched by their `#`-stripped and leaf-name forms so
  producer→consumer edges connect for qualified names.

* CTE names are scoped per statement: two statements each defining
  `WITH base AS (...)` no longer cross-wire, and a CTE in one statement
  no longer hides a same-named physical table referenced in another.

* Classification fixes: `CASE WHEN ... THEN DATEDIFF(...)` reads as
  `case` (structural shape outranks function matches); hyphenated
  literals like `'2024-01-01'` are no longer `arithmetic`; window
  aggregates (`SUM(x) OVER (...)`) no longer mislabel the stage as a
  grouped aggregate; stage labels name the specific date functions used.

* Fixed the colour collision where the join-key amber was the same hex as
  the aggregate pastel; "projected + key" is now visually distinct from
  "projected".

## Earlier development

### New features

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

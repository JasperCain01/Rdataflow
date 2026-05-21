# Rdataflow

**Rdataflow** visualises the column-level data flow of a SQL script. Given a
SQL script and optional database metadata, it parses the query, traces each
column from its source table through every CTE and join to the final output,
and renders an interactive flow diagram in the RStudio Viewer.

## Key features

- **Column-level lineage** — every column is tracked from source table to output
- **Full catalog view** — all table columns are shown; unused columns are visually dimmed
- **Join annotations** — join keys are highlighted; edge labels show join type (LEFT JOIN etc.)
- **Transformation badges** — each stage flags aggregations, date calculations, CASE expressions, window functions, and more
- **T-SQL first** — designed for SQL Server / T-SQL; other dialects supported via the `dialect` argument

## Installation

```r
# Install from source (development version)
devtools::install_github("jasper-cain/Rdataflow")
```

Rdataflow requires the Python `sqlglot` library. Install it once after loading the package:

```r
library(Rdataflow)
install_sqlglot()  # installs into reticulate's managed Python environment
```

## Quick start

```r
library(Rdataflow)

# 1. Describe the schema offline (or use schema_from_con() for a live DB)
s <- schema_from_list(list(
  "dbo.customers" = c(customer_id = "INT", name = "VARCHAR", region_id = "INT"),
  "dbo.orders"    = c(order_id = "INT", customer_id = "INT", amount = "DECIMAL"),
  "dbo.regions"   = c(region_id = "INT", region_name = "VARCHAR")
))

# 2. Write (or read) the SQL script
sql <- "
  WITH recent AS (
    SELECT o.customer_id, SUM(o.amount) AS total, COUNT(*) AS n
    FROM dbo.orders o
    GROUP BY o.customer_id
  )
  SELECT
    c.customer_id,
    r.region_name,
    recent.total,
    recent.n
  INTO #summary
  FROM dbo.customers c
  JOIN   recent          ON recent.customer_id = c.customer_id
  LEFT JOIN dbo.regions r ON r.region_id      = c.region_id
"

# 3. Render — displays in the RStudio Viewer
sql_dataflow(sql, schema = s)
```

## Reading a script from a file

```r
sql <- readLines("path/to/my_script.sql", warn = FALSE)
sql_dataflow(sql, schema = s)
```

## Live database connection (SQL Server via odbc)

```r
library(DBI)
library(odbc)

con <- dbConnect(odbc(), dsn = "my_dsn")
s   <- schema_from_con(con)          # reads INFORMATION_SCHEMA.COLUMNS
sql_dataflow(readLines("report.sql"), schema = s)
dbDisconnect(con)
```

## Structural overview mode

Pass `show_col_edges = FALSE` for a cleaner high-level view that shows
table-to-stage connections labelled with join types instead of individual
column lineage arrows:

```r
sql_dataflow(sql, schema = s, show_col_edges = FALSE)
```

## Step-by-step access

Each pipeline stage is exported individually for inspection or customisation:

```r
# Parse the SQL into a raw lineage list
parsed <- parse_sql(sql, schema = s)

# Build the typed IR (stages, projections, joins, ...)
ir <- build_ir(parsed)

# Annotate each projection with its transformation type
ir <- classify_transform(ir)

# Assemble the graph model (table nodes, stage nodes, edges)
graph <- build_graph(ir, schema = s)

# Render (returns a DiagrammeR widget)
plot_sqlflow(graph)

# Or inspect the raw DOT markup
cat(graph_to_dot(graph))
```

## Visual design

| Node type | Colour | Meaning |
|-----------|--------|---------|
| Table column — projected | light blue | column appears in SELECT |
| Table column — join key | light amber | column used in a JOIN ON |
| Table column — both | mid blue | projected and a join key |
| Table column — unused | near-white | in catalog but not referenced |
| Stage column | varies by type | transformation category |

Stage column colours by transformation type:

| Type | Colour | Examples |
|------|--------|---------|
| aggregate | amber | SUM, COUNT, AVG |
| window | teal | ROW_NUMBER, RANK |
| date | blue | DATEDIFF, DATEADD |
| case | purple | CASE WHEN ... END |
| cast | orange | CAST, CONVERT |
| string | pink | CONCAT, SUBSTRING |
| arithmetic | yellow | `col * 2`, `a + b` |
| passthrough | white | plain column reference |

## Supported SQL features

- CTEs (`WITH ... AS (...)`)
- `SELECT ... INTO #temp` and `INSERT INTO ... SELECT`
- `INNER`, `LEFT`, `RIGHT`, `FULL OUTER`, `CROSS` joins
- `GROUP BY`, `HAVING`, `WHERE`
- Window functions (`OVER (PARTITION BY ...)`)
- T-SQL date functions (`DATEDIFF`, `DATEADD`, `CONVERT`, ...)
- `CASE` expressions
- Multi-statement scripts (each statement is a separate flow)

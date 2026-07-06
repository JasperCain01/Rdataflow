# These tests exercise the live sqlglot path and are skipped when the Python
# dependency is unavailable (e.g. CI without a configured Python).

test_schema <- function() {
  schema_from_list(list(
    "dbo.customers" = c(customer_id = "INT", name = "VARCHAR", region_id = "INT"),
    "dbo.orders" = c(order_id = "INT", customer_id = "INT", amount = "DECIMAL")
  ))
}

test_that("parse_sql returns stages for a CTE + SELECT INTO", {
  skip_if_not(sqlglot_available(), "sqlglot not available")

  sql <- paste(
    "WITH recent AS (SELECT o.customer_id, SUM(o.amount) AS total",
    "FROM dbo.orders o GROUP BY o.customer_id)",
    "SELECT c.customer_id, recent.total INTO #summary",
    "FROM dbo.customers c JOIN recent ON recent.customer_id = c.customer_id"
  )

  res <- parse_sql(sql, schema = test_schema())
  st <- res$statements[[1]]

  expect_equal(st$kind, "select_into")
  expect_equal(length(st$stages), 2)
  roles <- vapply(st$stages, function(s) s$role, character(1))
  expect_setequal(roles, c("cte", "output"))
})

test_that("parse_sql expands * against the schema", {
  skip_if_not(sqlglot_available(), "sqlglot not available")

  res <- parse_sql("SELECT * FROM dbo.orders", schema = test_schema())
  outputs <- vapply(res$statements[[1]]$stages[[1]]$projections,
                    function(p) p$output, character(1))
  expect_setequal(outputs, c("order_id", "customer_id", "amount"))
})

# --- Batch A regression tests -----------------------------------------------

test_that("extract_output_table handles bracket-quoted identifiers", {
  expect_equal(
    extract_output_table("SELECT a INTO [dbo].[summary] FROM t", "select_into"),
    "dbo.summary"
  )
  expect_equal(
    extract_output_table("SELECT a INTO #tmp FROM t", "select_into"),
    "#tmp"
  )
  expect_equal(
    extract_output_table("INSERT INTO [dbo].[target] (a) SELECT a FROM t",
                         "insert_select"),
    "dbo.target"
  )
  expect_equal(
    extract_output_table("CREATE TABLE [dbo].[t2] (id INT)", "create_table"),
    "dbo.t2"
  )
})

# --- Batch E regression tests -----------------------------------------------

test_that("derived-table subqueries become their own stages", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  sql <- paste(
    "SELECT t.x, d.n FROM dbo.t",
    "JOIN (SELECT id, COUNT(*) AS n FROM dbo.u GROUP BY id) d ON d.id = t.id"
  )
  res <- parse_sql(sql)
  stages <- res$statements[[1]]$stages
  roles <- vapply(stages, function(s) s$role, character(1))
  expect_true("subquery" %in% roles)

  sub <- stages[[which(roles == "subquery")]]
  expect_equal(sub$name, "d")
  # inner table belongs to the subquery stage, not the outer stage
  expect_equal(vapply(sub$sources, function(s) s$table, character(1)), "u")
  out <- stages[[which(roles == "output")]]
  outer_tables <- vapply(out$sources, function(s) s$table, character(1))
  expect_setequal(outer_tables, c("t", "d"))
})

test_that("UNION extracts every branch as an output stage", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  res <- parse_sql("SELECT a FROM dbo.t UNION ALL SELECT a FROM dbo.u")
  stages <- res$statements[[1]]$stages
  expect_equal(length(stages), 2L)
  tabs <- vapply(stages, function(s) s$sources[[1]]$table, character(1))
  expect_setequal(tabs, c("t", "u"))
})

test_that("INSERT column list renames the output columns", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  res <- parse_sql("INSERT INTO dbo.tgt (c1, c2) SELECT a, b FROM dbo.src")
  proj <- res$statements[[1]]$stages[[1]]$projections
  outs <- vapply(proj, function(p) p$output, character(1))
  expect_equal(outs, c("c1", "c2"))
})

test_that("CROSS APPLY becomes a subquery stage with an APPLY join", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  sql <- "SELECT t.x, ca.v FROM dbo.t CROSS APPLY (SELECT v FROM dbo.u WHERE u.id = t.id) ca"
  res <- parse_sql(sql)
  stages <- res$statements[[1]]$stages
  roles <- vapply(stages, function(s) s$role, character(1))
  expect_true("subquery" %in% roles)
  out <- stages[[which(roles == "output")]]
  expect_equal(out$joins[[1]]$kind, "APPLY")
})

# --- Batch H regression tests (indirect lineage: condition/partition/filter) -

test_that("CASE WHEN predicate columns are 'condition', THEN/ELSE columns 'value'", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  sql <- "SELECT CASE WHEN status = 1 THEN amount ELSE 0 END AS adj FROM dbo.t"
  res <- parse_sql(sql)
  proj <- res$statements[[1]]$stages[[1]]$projections[[1]]
  cols <- proj$columns
  roles_by_name <- stats::setNames(
    vapply(cols, function(c) c$role, character(1)),
    vapply(cols, function(c) c$name, character(1))
  )
  expect_equal(roles_by_name[["status"]], "condition")
  expect_equal(roles_by_name[["amount"]], "value")
})

test_that("window PARTITION BY / ORDER BY columns are 'partition'", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  sql <- "SELECT SUM(x) OVER (PARTITION BY grp ORDER BY d) AS running FROM dbo.t"
  res <- parse_sql(sql)
  proj <- res$statements[[1]]$stages[[1]]$projections[[1]]
  cols <- proj$columns
  roles_by_name <- stats::setNames(
    vapply(cols, function(c) c$role, character(1)),
    vapply(cols, function(c) c$name, character(1))
  )
  expect_equal(roles_by_name[["x"]], "value")
  expect_equal(roles_by_name[["grp"]], "partition")
  expect_equal(roles_by_name[["d"]], "partition")
})

test_that("WHERE-clause columns are captured as where_columns", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  res <- parse_sql("SELECT a FROM dbo.t WHERE b > 5")
  stg <- res$statements[[1]]$stages[[1]]
  where_cols <- vapply(stg$where_columns, function(c) c$name, character(1))
  expect_equal(where_cols, "b")
})

# --- Batch I regression tests (MERGE / UPDATE support) ----------------------

merge_update_schema <- function() {
  schema_from_list(list(
    "dbo.target" = c(id = "INT", amount = "DECIMAL", name = "VARCHAR"),
    "dbo.source" = c(id = "INT", amount = "DECIMAL", name = "VARCHAR")
  ))
}

test_that("plain UPDATE produces a single output stage from SET assignments", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  res <- parse_sql("UPDATE dbo.target SET amount = 5 WHERE id = 1",
                   schema = merge_update_schema())
  st <- res$statements[[1]]
  expect_equal(st$kind, "update")
  expect_equal(length(st$stages), 1L)
  expect_equal(st$stages[[1]]$role, "output")
  outs <- vapply(st$stages[[1]]$projections, function(p) p$output, character(1))
  expect_equal(outs, "amount")
})

test_that("UPDATE ... FROM ... JOIN names projections from SET targets and traces sources", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  sql <- paste(
    "UPDATE dbo.target",
    "SET amount = s.amount, name = s.name",
    "FROM dbo.target t JOIN dbo.source s ON s.id = t.id",
    "WHERE t.id > 0"
  )
  res <- parse_sql(sql, schema = merge_update_schema())
  st <- res$statements[[1]]
  expect_equal(st$output_table, "dbo.target")

  stg <- st$stages[[1]]
  outs <- vapply(stg$projections, function(p) p$output, character(1))
  expect_setequal(outs, c("amount", "name"))

  src_tables <- vapply(stg$sources, function(s) s$table, character(1))
  expect_setequal(src_tables, c("target", "source"))

  amount_cols <- stg$projections[[which(outs == "amount")]]$columns
  expect_equal(vapply(amount_cols, function(c) c$name, character(1)), "amount")

  where_cols <- vapply(stg$where_columns, function(c) c$name, character(1))
  expect_equal(where_cols, "id")
})

test_that("MERGE captures USING source, ON join keys, and WHEN branch projections", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  sql <- paste(
    "MERGE INTO dbo.target AS tgt",
    "USING dbo.source AS src ON tgt.id = src.id",
    "WHEN MATCHED THEN UPDATE SET tgt.amount = src.amount, tgt.name = src.name",
    "WHEN NOT MATCHED THEN INSERT (id, amount, name) VALUES (src.id, src.amount, src.name)"
  )
  res <- parse_sql(sql, schema = merge_update_schema())
  st <- res$statements[[1]]
  expect_equal(st$kind, "merge")
  expect_equal(st$output_table, "dbo.target")
  expect_equal(length(st$stages), 1L)

  stg <- st$stages[[1]]
  src_tables <- vapply(stg$sources, function(s) s$table, character(1))
  expect_setequal(src_tables, c("target", "source"))

  outs <- vapply(stg$projections, function(p) p$output, character(1))
  expect_setequal(outs, c("amount", "name", "id"))

  expect_equal(length(stg$joins), 1L)
  keys <- stg$joins[[1]]$keys
  expect_equal(length(keys), 1L)
  expect_equal(keys[[1]]$left, "tgt.id")
  expect_equal(keys[[1]]$right, "src.id")
})

test_that("MERGE INSERT without an explicit column list falls back to source column names", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  sql <- paste(
    "MERGE INTO dbo.target AS tgt",
    "USING dbo.source AS src ON tgt.id = src.id",
    "WHEN NOT MATCHED THEN INSERT VALUES (src.id, src.amount, src.name)"
  )
  res <- parse_sql(sql, schema = merge_update_schema())
  outs <- vapply(res$statements[[1]]$stages[[1]]$projections,
                 function(p) p$output, character(1))
  expect_setequal(outs, c("id", "amount", "name"))
})

test_that("extract_output_table sees through leading comments", {
  expect_equal(
    extract_output_table("/* make temp */\nCREATE TABLE #t (a INT)", "create_table"),
    "#t"
  )
  expect_equal(
    extract_output_table("-- append\nINSERT INTO dbo.x (a) SELECT a FROM t",
                         "insert_select"),
    "dbo.x"
  )
  expect_equal(
    extract_output_table("/*x*/ MERGE INTO dbo.t USING s ON 1=1;", "merge"),
    "dbo.t"
  )
  expect_equal(
    extract_output_table("-- upd\nUPDATE dbo.t SET a = 1", "update"),
    "dbo.t"
  )
  # INTO inside a comment must not be mistaken for SELECT ... INTO.
  expect_null(
    extract_output_table("-- goes into the log\nSELECT a FROM t", "select_into")
  )
})

test_that("variables declared under a header comment substitute downstream", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  sql <- paste(
    "/* nightly job */",
    "DECLARE @cut DATE = '2025-06-30';",
    "SELECT o.id INTO #x FROM dbo.orders o WHERE o.sold_on <= @cut;",
    sep = "\n"
  )
  s <- schema_from_list(list("dbo.orders" = c(id = "INT", sold_on = "DATE")))
  parsed <- parse_sql(sql, schema = s)
  expect_false(any(grepl("unresolved variable", parsed$skipped)))
  st <- parsed$statements[[1]]
  expect_match(st$stages[[1]]$where, "2025-06-30", fixed = TRUE)
})

test_that("UPDATE via a FROM alias resolves to the underlying table", {
  skip_if_not(sqlglot_available(), "sqlglot not available")
  s <- schema_from_list(list(
    "dbo.orders" = c(order_id = "INT", cust_id = "INT", status = "VARCHAR"),
    "dbo.cust"   = c(cust_id = "INT", tier = "VARCHAR")
  ))
  sql <- "
    UPDATE o
    SET o.status = 'big'
    FROM dbo.orders o
    JOIN dbo.cust c ON c.cust_id = o.cust_id
    WHERE c.tier = 'gold'
  "
  parsed <- parse_sql(sql, schema = s)
  st <- parsed$statements[[1]]
  expect_equal(st$kind, "update")
  expect_match(st$output_table, "orders")
  expect_match(st$stages[[1]]$name, "orders")
})

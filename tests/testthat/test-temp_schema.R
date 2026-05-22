test_that("parse_create_table_columns extracts column name and type", {
  sql <- "CREATE TABLE #t (id INT NOT NULL, name VARCHAR(100), dt DATETIME)"
  res <- parse_create_table_columns(sql)
  expect_equal(res$column, c("id", "name", "dt"))
  expect_equal(res$type,   c("INT", "VARCHAR", "DATETIME"))
  expect_equal(res$ordinal, 1:3)
})

test_that("parse_create_table_columns handles NOT NULL and PRIMARY KEY noise", {
  sql <- "CREATE TABLE #t (id INT NOT NULL PRIMARY KEY, val DECIMAL(10,2) NOT NULL)"
  res <- parse_create_table_columns(sql)
  expect_equal(res$column, c("id", "val"))
  expect_equal(res$type,   c("INT", "DECIMAL"))
})

test_that("parse_create_table_columns handles bracket-quoted column names", {
  sql <- "CREATE TABLE #t ([order id] INT, [my col] NVARCHAR(50))"
  res <- parse_create_table_columns(sql)
  expect_equal(res$column, c("order id", "my col"))
})

test_that("parse_create_table_columns returns empty tibble when no parens", {
  res <- parse_create_table_columns("CREATE TABLE #t")
  expect_equal(nrow(res), 0L)
  expect_equal(names(res), c("column", "type", "ordinal"))
})

test_that("new_temp_registry returns zero-row typed tibble", {
  reg <- new_temp_registry()
  expect_equal(nrow(reg), 0L)
  expect_equal(names(reg), c("table", "column", "type", "ordinal", "origin_seq"))
})

test_that("register_temp_table appends rows to registry", {
  reg <- new_temp_registry()
  cols <- parse_create_table_columns(
    "CREATE TABLE #t (id INT, name VARCHAR(50))"
  )
  reg <- register_temp_table(reg, "#t", cols, origin_seq = 1L)
  expect_equal(nrow(reg), 2L)
  expect_equal(reg$table, c("#t", "#t"))
  expect_equal(reg$column, c("id", "name"))
  expect_equal(reg$origin_seq, c(1L, 1L))
})

test_that("register_temp_table is a no-op for zero-row columns_tbl", {
  reg <- new_temp_registry()
  empty_cols <- tibble::tibble(column = character(), type = character(),
                               ordinal = integer())
  reg2 <- register_temp_table(reg, "#t", empty_cols, origin_seq = 1L)
  expect_equal(nrow(reg2), 0L)
})

test_that("merge_temp_schema includes temp columns in returned schema", {
  reg <- new_temp_registry()
  cols <- parse_create_table_columns(
    "CREATE TABLE #prison_terms (prison_id INT, offender_id INT)"
  )
  reg <- register_temp_table(reg, "#prison_terms", cols, origin_seq = 1L)

  sql <- "SELECT p.prison_id FROM #prison_terms p"
  merged <- merge_temp_schema(sql, schema = NULL, temp_registry = reg)

  expect_s3_class(merged, "rdataflow_schema")
  expect_true("prison_id" %in% merged$columns$column)
})

test_that("merge_temp_schema returns NULL schema unchanged when no temp match", {
  reg <- new_temp_registry()
  cols <- parse_create_table_columns("CREATE TABLE #other (id INT)")
  reg <- register_temp_table(reg, "#other", cols, origin_seq = 1L)

  # SQL references #prison_terms, not #other — no match
  sql <- "SELECT p.id FROM #prison_terms p"
  result <- merge_temp_schema(sql, schema = NULL, temp_registry = reg)
  expect_null(result)
})

test_that("merge_temp_schema uses latest registration for a re-defined temp", {
  reg <- new_temp_registry()

  cols1 <- tibble::tibble(column = c("a"), type = c("INT"), ordinal = 1L)
  reg <- register_temp_table(reg, "#t", cols1, origin_seq = 1L)

  cols2 <- tibble::tibble(column = c("a", "b"), type = c("INT", "VARCHAR"),
                          ordinal = 1:2)
  reg <- register_temp_table(reg, "#t", cols2, origin_seq = 5L)

  sql <- "SELECT * FROM #t"
  merged <- merge_temp_schema(sql, schema = NULL, temp_registry = reg)

  # Only the origin_seq=5 set should be present — both "a" and "b"
  expect_setequal(merged$columns$column, c("a", "b"))
})

test_that("merge_temp_schema merges with existing schema", {
  base_schema <- schema_from_list(list(
    "dbo.patients" = c(patient_id = "INT", name = "VARCHAR")
  ))
  reg <- new_temp_registry()
  cols <- parse_create_table_columns("CREATE TABLE #t (id INT, val FLOAT)")
  reg <- register_temp_table(reg, "#t", cols, origin_seq = 2L)

  sql <- "SELECT p.patient_id, t.id FROM dbo.patients p JOIN #t t ON p.patient_id = t.id"
  merged <- merge_temp_schema(sql, schema = base_schema, temp_registry = reg)

  all_tables <- unique(merged$columns$table)
  expect_true("dbo.patients" %in% all_tables || "patients" %in% all_tables)
  expect_true("#t" %in% all_tables)
})

test_that("forward reference (temp not yet registered) degrades to no schema", {
  reg <- new_temp_registry()  # empty — #t not yet defined

  sql <- "SELECT * FROM #t"
  result <- merge_temp_schema(sql, schema = NULL, temp_registry = reg)
  expect_null(result)
})

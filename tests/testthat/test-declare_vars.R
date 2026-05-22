test_that("extract_declare parses a DECLARE with literal date value", {
  res <- extract_declare("DECLARE @StartDate DATE = '2024-01-01'")
  expect_equal(res$name,       "@StartDate")
  expect_equal(res$type,       "DATE")
  expect_equal(res$value_expr, "'2024-01-01'")
  expect_true(res$is_literal)
})

test_that("extract_declare parses a DECLARE with no default value", {
  res <- extract_declare("DECLARE @RunDate DATETIME")
  expect_equal(res$name,       "@RunDate")
  expect_equal(res$type,       "DATETIME")
  expect_true(is.na(res$value_expr))
  expect_false(res$is_literal)
})

test_that("extract_declare parses a DECLARE with complex expression", {
  res <- extract_declare(
    "DECLARE @RunDate DATETIME = DATEADD(day, 0, GETDATE())"
  )
  expect_equal(res$name, "@RunDate")
  expect_equal(res$type, "DATETIME")
  expect_false(res$is_literal)
})

test_that("extract_declare parses DECLARE with VARCHAR type and precision", {
  res <- extract_declare("DECLARE @Name VARCHAR(100) = 'Alice'")
  expect_equal(res$type,       "VARCHAR")
  expect_equal(res$value_expr, "'Alice'")
  expect_true(res$is_literal)
})

test_that("extract_declare parses SET statement", {
  res <- extract_declare("SET @RunDate = GETDATE()")
  expect_equal(res$name,       "@RunDate")
  expect_true(is.na(res$type))
  expect_false(res$is_literal)
})

test_that("extract_declare returns empty tibble for non-DECLARE statements", {
  res <- extract_declare("SELECT * FROM dbo.patients")
  expect_equal(nrow(res), 0L)
  expect_equal(names(res), c("name", "type", "value_expr", "is_literal"))
})

test_that("extract_declare handles integer literal", {
  res <- extract_declare("DECLARE @Limit INT = 100")
  expect_equal(res$type,       "INT")
  expect_equal(res$value_expr, "100")
  expect_true(res$is_literal)
})

test_that("substitute_vars replaces literal @var verbatim", {
  vars <- extract_declare("DECLARE @StartDate DATE = '2024-01-01'")
  sql  <- "SELECT * FROM t WHERE dt >= @StartDate"
  out  <- substitute_vars(sql, vars)
  expect_equal(out, "SELECT * FROM t WHERE dt >= '2024-01-01'")
})

test_that("substitute_vars replaces non-literal with typed sentinel", {
  vars <- extract_declare(
    "DECLARE @RunDate DATETIME = DATEADD(day, -1, GETDATE())"
  )
  sql  <- "SELECT * FROM t WHERE dt = @RunDate"
  out  <- substitute_vars(sql, vars)
  expect_equal(out, "SELECT * FROM t WHERE dt = '1900-01-01 00:00:00'")
})

test_that("substitute_vars replaces INT non-literal with 0", {
  vars <- extract_declare("DECLARE @N INT = (SELECT COUNT(*) FROM t)")
  sql  <- "SELECT TOP @N id FROM t"
  out  <- substitute_vars(sql, vars)
  expect_equal(out, "SELECT TOP 0 id FROM t")
})

test_that("substitute_vars is word-boundary: @Start does not match @StartDate", {
  vars <- tibble::tibble(
    name = "@Start", type = "DATE", value_expr = "'2024-01-01'",
    is_literal = TRUE
  )
  sql <- "WHERE dt >= @StartDate AND t >= @Start"
  out <- substitute_vars(sql, vars)
  # Only @Start (the exact match) should be replaced
  expect_true(grepl("@StartDate", out))
  expect_false(grepl("@Start\\b", out, perl = TRUE))
})

test_that("substitute_vars handles multiple variables in one statement", {
  vars <- dplyr::bind_rows(
    extract_declare("DECLARE @A INT = 1"),
    extract_declare("DECLARE @B VARCHAR(10) = 'hello'")
  )
  sql <- "WHERE id = @A AND name = @B"
  out <- substitute_vars(sql, vars)
  expect_equal(out, "WHERE id = 1 AND name = 'hello'")
})

test_that("substitute_vars leaves @var unchanged when type unknown and non-literal", {
  vars <- extract_declare("SET @X = SOME_FUNC()")
  sql  <- "SELECT @X AS val"
  out  <- substitute_vars(sql, vars)
  # No type → no sentinel known → leave as-is
  expect_equal(out, sql)
})

test_that("is_literal detects CAST(literal AS type) as literal", {
  vars <- tibble::tibble(
    name = "@D", type = "DATE",
    value_expr = "CAST('2024-01-01' AS DATE)",
    is_literal = TRUE
  )
  sql <- "WHERE dt = @D"
  out <- substitute_vars(sql, vars)
  expect_equal(out, "WHERE dt = CAST('2024-01-01' AS DATE)")
})

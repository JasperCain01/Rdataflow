test_that("schema_from_list parses unnamed column vectors", {
  s <- schema_from_list(list(
    "dbo.customers" = c("customer_id", "name", "region_id")
  ))

  expect_s3_class(s, "rdataflow_schema")
  expect_equal(nrow(s$columns), 3)
  expect_equal(s$columns$schema, rep("dbo", 3))
  expect_equal(s$columns$table, rep("customers", 3))
  expect_equal(s$columns$column, c("customer_id", "name", "region_id"))
  expect_true(all(is.na(s$columns$type)))
  expect_equal(s$columns$ordinal, 1:3)
})

test_that("schema_from_list captures types from named vectors", {
  s <- schema_from_list(list(
    "dbo.regions" = c(region_id = "INT", region_name = "VARCHAR(50)")
  ))

  expect_equal(s$columns$column, c("region_id", "region_name"))
  expect_equal(s$columns$type, c("INT", "VARCHAR(50)"))
})

test_that("table identifiers are parsed into catalog/schema/table", {
  expect_equal(
    parse_table_identifier("customers"),
    list(catalog = NA_character_, schema = NA_character_, table = "customers")
  )
  expect_equal(
    parse_table_identifier("dbo.customers"),
    list(catalog = NA_character_, schema = "dbo", table = "customers")
  )
  expect_equal(
    parse_table_identifier("sales.dbo.orders"),
    list(catalog = "sales", schema = "dbo", table = "orders")
  )
})

test_that("bracket-quoted identifiers are stripped and dot-safe", {
  expect_equal(
    parse_table_identifier("[dbo].[customers]"),
    list(catalog = NA_character_, schema = "dbo", table = "customers")
  )
  # A dot inside brackets must not be treated as a separator.
  expect_equal(
    parse_table_identifier("[dbo].[my.table]"),
    list(catalog = NA_character_, schema = "dbo", table = "my.table")
  )
})

test_that("multiple tables stack into one catalog", {
  s <- schema_from_list(list(
    "dbo.customers" = c("customer_id", "name"),
    "dbo.orders"    = c("order_id", "customer_id", "amount")
  ))
  expect_equal(nrow(s$columns), 5)
  expect_setequal(unique(s$columns$table), c("customers", "orders"))
})

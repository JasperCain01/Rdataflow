test_that("parse_script classifies imports and transforms", {
  code <- c(
    'raw <- readr::read_csv("data.csv")',
    'clean <- raw |> dplyr::filter(x > 0)',
    'summary_df <- clean |> dplyr::summarise(mean_x = mean(x))'
  )

  parsed <- parse_script(code)

  expect_equal(nrow(parsed), 3)
  expect_equal(parsed$target, c("raw", "clean", "summary_df"))
  expect_equal(parsed$kind, c("import", "transform", "transform"))
  expect_equal(parsed$call_fn, c("read_csv", "filter", "summarise"))
  expect_true("raw" %in% parsed$inputs[[2]])
  expect_true("clean" %in% parsed$inputs[[3]])
})

test_that("parse_script ignores non-assignment top-level calls", {
  code <- c(
    'x <- 1',
    'print(x)'
  )
  parsed <- parse_script(code)
  expect_equal(parsed$target, "x")
})

test_that("parse_script produces a human-readable summary per step", {
  code <- c(
    'raw <- readr::read_csv("data.csv")',
    'clean <- raw |> dplyr::filter(x > 0)',
    'final <- clean |> dplyr::mutate(y = x * 2)'
  )

  parsed <- parse_script(code)

  expect_equal(parsed$summary[1], 'read_csv: "data.csv"')
  expect_equal(parsed$summary[2], "filter: x > 0")
  expect_equal(parsed$summary[3], "mutate: y")
})

test_that("trace_dataflow attaches column names from env", {
  raw <- tibble::tibble(a = 1:2, b = 3:4)
  clean <- dplyr::filter(raw, a > 0)
  model_data <- dplyr::mutate(clean, c = a + b)

  code <- c(
    'raw <- readr::read_csv("data.csv")',
    'clean <- raw |> dplyr::filter(a > 0)',
    'model_data <- clean |> dplyr::mutate(c = a + b)'
  )

  lineage <- trace_dataflow(model_data, script = code, plot = FALSE,
                            env = environment())

  cols <- setNames(lineage$nodes$columns, lineage$nodes$id)
  expect_equal(cols$raw, c("a", "b"))
  expect_equal(cols$clean, c("a", "b"))
  expect_equal(cols$model_data, c("a", "b", "c"))
})

test_that("missing objects fall back to empty columns", {
  code <- c(
    'raw <- readr::read_csv("data.csv")',
    'clean <- raw |> dplyr::filter(x > 0)'
  )

  lineage <- trace_dataflow("clean", script = code, plot = FALSE,
                            env = new.env(parent = emptyenv()))

  cols <- lineage$nodes$columns
  expect_true(all(vapply(cols, length, integer(1)) == 0L))
})

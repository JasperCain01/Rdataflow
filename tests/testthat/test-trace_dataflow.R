test_that("trace_dataflow restricts lineage to ancestry of target", {
  code <- c(
    'raw <- readr::read_csv("data.csv")',
    'other <- readr::read_csv("other.csv")',
    'clean <- raw |> dplyr::filter(x > 0)',
    'model_data <- clean |> dplyr::mutate(y = x * 2)'
  )

  lineage <- trace_dataflow(model_data, script = code, plot = FALSE)

  expect_setequal(lineage$nodes$id, c("raw", "clean", "model_data"))
  expect_false("other" %in% lineage$nodes$id)
  expect_equal(nrow(lineage$edges), 2)
})

test_that("trace_dataflow errors when target is not defined", {
  expect_error(
    trace_dataflow(nope, script = 'x <- 1', plot = FALSE),
    "Could not find an assignment"
  )
})

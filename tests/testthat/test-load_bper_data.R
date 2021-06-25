
bper_data_test <- load_bper_data(example_persons)

test_that("bper data loads", {
  expect_is(bper_data_test, "list")
})

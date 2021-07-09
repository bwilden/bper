
bper_data_test <- load_bper_data(example_persons, year = 2010)

test_that("bper data loads", {
  expect_is(bper_data_test, "list")
})


test_persons <- example_persons %>%
  select(first_name, party, state)

bper_data_test <- load_bper_data(input_data = test_persons,
                                 year = 2010,
                                 census_key = "5e4c2b8438222753a7f4753fa78855eca73b9950")

test_that("bper data loads", {
  expect_type(bper_data_test, "list")
})

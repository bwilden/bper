
test_persons <- example_persons %>%
  select(first_name, party, state)

imputations_test <-
  impute_ethnorace(input_data = test_persons,
                   year = 2010,
                   census_key = "5e4c2b8438222753a7f4753fa78855eca73b9950")

test_that("returns a data.frame object", {
  expect_s3_class(imputations_test, "data.frame")
})

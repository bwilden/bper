

predictions_test <-
  predict_ethnorace(example_persons, geo = "state")

test_that("returns a data.frame object", {
  expect_is(predictions_test, "data.frame")
})



test_df <- predict_race(example_persons)
test_df_dichot <- predict_race(example_persons, dichotomize = T)

test_that("returns a data.frame object", {
  expect_is(test_df, "data.frame")
})

test_that("pred_race column contains only valid ethnorace categories", {
  ethnoraces <- c("aian", "api", "black", "hispanic", "white", "other")
  matched_ethnoraces <- test_df$pred_race %in% ethnoraces
  true_vec <- rep(TRUE, nrow(test_df))

  expect_equal(matched_ethnoraces, true_vec)
})

test_that("arg_max_cols chooses correct column", {
  thin_df <- test_df[, -ncol(test_df)]
  thin_df <- arg_max_cols(thin_df, 6, max_col_name = "pred_race")

  expect_equal(thin_df$pred_race, paste0("prob_", test_df$pred_race))
})

test_that("dichotomize argument returns extra columns", {
  num_col_orig <- ncol(example_persons)
  num_col_post <- ncol(test_df_dichot)

  expect_equal(num_col_post, num_col_orig + 14)
})

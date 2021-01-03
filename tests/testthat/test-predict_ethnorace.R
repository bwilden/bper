
load_bperdata("bperdata", download = FALSE)

ethnorace_set <- c("aian", "api", "black", "hispanic", "white", "other")

# Basic -------------------------------------------------------------------

test_df <- predict_ethnorace(example_persons)

test_that("returns a data.frame object", {
  expect_is(test_df, "data.frame")
})

test_that("pred_race column contains only valid ethnorace categories", {
  matched_ethnoraces <- test_df$pred_race %in% ethnorace_set

  expect_equal(matched_ethnoraces, rep(TRUE, nrow(test_df)))
})

test_that("arg_max_cols chooses correct column", {
  thin_df <- test_df[, -ncol(test_df)]
  thin_df <- arg_max_cols(thin_df, 6, max_col_name = "pred_race")

  expect_equal(thin_df$pred_race, paste0("prob_", test_df$pred_race))
})


# Dichotomize feature -----------------------------------------------------

test_df_dichot <- predict_ethnorace(example_persons, dichotomize = T)

test_that("dichotomize returns extra columns with correct names", {
  num_col_orig <- ncol(example_persons)
  num_col_post <- ncol(test_df_dichot)

  expect_equal(num_col_post, num_col_orig + 13)
  expect_equal(ethnorace_set %in% colnames(test_df_dichot),
               rep(TRUE, length(ethnorace_set)))
})


# Input data problems -----------------------------------------------------

test_that("input data missing column(s) still computes predictions", {
  test_df <-
    example_persons %>% dplyr::select(-c(first_name, female)) %>%
    predict_ethnorace()

  expect_is(test_df, "data.frame")
  expect_equal(test_df$pred_race %in% ethnorace_set, rep(TRUE, nrow(test_df)))
})

test_that("input data with wrong col types still computes predictions", {
  test_df <-
    example_persons %>% dplyr::mutate(zip = as.numeric(zip)) %>%
    predict_ethnorace()

  expect_is(test_df, "data.frame")
  expect_equal(test_df$pred_race %in% ethnorace_set, rep(TRUE, nrow(test_df)))
})

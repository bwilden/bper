


test_people <- data.frame(first_name = c("MOLLY", "DESHAWN", "ENGELBERT"),
                          last_name = c("MUELLER", "JACKSON", "HUMPERDINCK"),
                          birth_year = c(1992, 1959, 1963),
                          female = c(1, 0, 0),
                          party = c("DEM", "DEM", "UNA"),
                          apartment = c(1, 0, 0),
                          zip = c("92092", "70113", "90210"),
                          block = c("010010201001000", NA_character_, NA_character_),
                          id = c(1, 2, 3))
test_df <- predict_race(test_people)
test_df_dichot <- predict_race(test_people, dichotomize = T)

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
  num_col_orig <- ncol(test_people)
  num_col_post <- ncol(test_df_dichot)

  expect_equal(num_col_post, num_col_orig + 13)
})

test_that("dichotomize argument categorizes correctly", {
  expect_equal(as.character(test_df_dichot$white), c("1", "0", "1"))
})

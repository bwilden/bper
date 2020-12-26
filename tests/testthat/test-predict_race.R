


test_people <- data.frame(first_name = c("JAKE", "DESHAWN"),
                          last_name = c("MUELLER", "JACKSON"),
                          birth_year = c(1992, 1959),
                          female = c(0, 0),
                          party = c("DEM", "DEM"),
                          apartment = c(1, 0),
                          zip = c("92092", "70113"),
                          block = c("010010201001000", NA_character_),
                          id = c(1, 2))
test_df <- predict_race(test_people)
test_df

test_that("returns a data.frame object", {
  expect_is(test_df, "data.frame")
})

test_that("arg_max_cols chooses correct column", {
  thin_df <- test_df[, -ncol(test_df)]
  thin_df <- arg_max_cols(thin_df, 6, max_col_name = "pred_race")

  expect_equal(thin_df$pred_race, paste0("prob_", test_df$pred_race))
})

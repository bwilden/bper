
df <- data.frame(a = c(0.3, 0.5),
                 b = c(0.2, 0.7),
                 c = c(0.1, 0.8))

test_that("arg_max_cols() returns appropriate data frame", {
  arg_max_df <- arg_max_cols(df, num_cols = ncol(df))

  expect_is(arg_max_df, "data.frame")
  expect_is(arg_max_df$max_col, "character")
  expect_equal(ncol(df) + 1, ncol(arg_max_df))
})

test_that("arg_max_cols() picks column with largest value", {
  arg_max_df <- arg_max_cols(df, num_cols = ncol(df))

  expect_equal(c("a", "c"), arg_max_df$max_col)
})

test_that("arg_max_cols() picks column with largest value in restricted range", {
  arg_max_df <- arg_max_cols(df, num_cols = ncol(df) - 1)

  expect_equal(c("b", "c"), arg_max_df$max_col)
})

test_that("arg_max_cols() names correct column", {
  arg_max_df <- arg_max_cols(df, num_cols = ncol(df), max_col_name = "top_dog")

  expect_equal("top_dog", colnames(arg_max_df)[ncol(arg_max_df)])
})

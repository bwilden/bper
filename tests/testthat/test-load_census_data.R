
last_names <- load_surnames_data(year = 2010)

test_that("surnames data loads", {
  expect_is(last_names, "data.frame")
})

test_that("surnames data has correct columns", {
  expect_equal(colnames(last_names),
               c("last_name", "pr_white|last",
                 "pr_black|last", "pr_aian|last",
                 "pr_api|last", "pr_hispanic|last",
                 "pr_other|last", "pr_last|white",
                 "pr_last|black", "pr_last|aian",
                 "pr_last|api", "pr_last|hispanic",
                 "pr_last|other"))
})

sex_age_totals <- get_sex_age_data("PCT12", "total", 2010)

test_that("sex_age_totals data loads", {
  expect_is(sex_age_totals, "data.frame")
})

sex_age <- load_sex_age_data(year = 2010, vars = "both")

test_that("sex_age_totals data loads", {
  expect_is(sex_age, "data.frame")
})

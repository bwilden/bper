
# Last Names --------------------------------------------------------------

last_names_test <- load_surnames_data(year = 2010)

test_that("surnames data loads", {
  expect_is(last_names_test, "data.frame")
})

test_that("surnames data has correct columns", {
  expect_equal(colnames(last_names_test),
               c("last_name", "pr_white|last",
                 "pr_black|last", "pr_aian|last",
                 "pr_api|last", "pr_hispanic|last",
                 "pr_other|last", "pr_last|white",
                 "pr_last|black", "pr_last|aian",
                 "pr_last|api", "pr_last|hispanic",
                 "pr_last|other"))
})


# Sex/Age -----------------------------------------------------------------

sex_ages_test <- load_sex_age_data(year = 2010, vars = "both",
                                  list(
                                    c("PCT12H", "hispanic"),
                                    c("PCT12I", "white"),
                                    c("PCT12J", "black")
                                  ))

test_that("sex_ages data loads", {
  expect_is(sex_ages_test, "data.frame")
})


# Multi-Unit Occupancy ----------------------------------------------------

multi_unit_test <- load_multi_unit_data(year = 2019,
                                        census_groups = list(
                                          c("B25032H", "white"),
                                          c("B25032I", "hispanic"),
                                          c("B25032B", "black")
                                        ))

test_that("multi_unit data loads", {
  expect_is(multi_unit_test, "data.frame")
})

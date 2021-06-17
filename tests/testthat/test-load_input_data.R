
# Last Names --------------------------------------------------------------

last_names_test <- load_surnames_data()

test_that("surnames data loads", {
  expect_is(last_names_test, "data.frame")
})


# First Names -------------------------------------------------------------

first_names_test <- load_first_names_data()

test_that("first names data loads", {
  expect_is(first_names_test, "data.frame")
})


# Party ID ----------------------------------------------------------------

parties_test <- load_parties_data()

test_that("parties data loads", {
  expect_is(parties_test, "data.frame")
})


# Multi-Unit Occupancy ----------------------------------------------------

multi_unit_test <- load_multi_unit_data()

test_that("multi_unit data loads", {
  expect_is(multi_unit_test, "data.frame")
})


# Sex/Age -----------------------------------------------------------------

sex_ages_test <- load_sex_age_data()

test_that("sex_ages data loads", {
  expect_is(sex_ages_test, "list")
  expect_is(sex_ages_test$sex_ages, "data.frame")
  expect_is(sex_ages_test$sexes, "data.frame")
  expect_is(sex_ages_test$ages, "data.frame")
})


# Geo ---------------------------------------------------------------------

geos_test <- load_geo_data("state")

test_that("geos data loads", {
  expect_is(geos_test, "data.frame")
})


Sys.setenv(CENSUS_KEY = "5e4c2b8438222753a7f4753fa78855eca73b9950")

# Last Names --------------------------------------------------------------

last_names_test <- load_surnames_data(year = 2010)

test_that("surnames data loads", {
  expect_s3_class(last_names_test, "data.frame")
})


# First Names -------------------------------------------------------------

first_names_test <- load_first_names_data()

test_that("first names data loads", {
  expect_s3_class(first_names_test, "data.frame")
})


# Party ID ----------------------------------------------------------------

parties_test <- load_parties_data(year = 2010)

test_that("parties data loads", {
  expect_s3_class(parties_test, "data.frame")
})


# Multi-Unit Occupancy ----------------------------------------------------

multi_unit_test <- load_multi_unit_data(year = 2010)

test_that("multi_unit data loads", {
  expect_s3_class(multi_unit_test, "data.frame")
})


# Sex/Age -----------------------------------------------------------------

sex_ages_test <- load_sex_age_data(year = 2010)

test_that("sex_ages data loads", {
  expect_type(sex_ages_test, "list")
  expect_s3_class(sex_ages_test$sex_ages, "data.frame")
  expect_s3_class(sex_ages_test$sexes, "data.frame")
  expect_s3_class(sex_ages_test$ages, "data.frame")
})


# Geo ---------------------------------------------------------------------

geos_test <- load_geo_data(geo_level = "state", year = 2010)

test_that("geos data loads", {
  expect_s3_class(geos_test, "data.frame")
})

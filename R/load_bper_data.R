
#' Load data for \code{\link{impute_ethnorace}} function
#'
#' This function retrieves the demographic data for use in
#' \code{\link{impute_ethnorace}}.
#'
#' @param input_data The input data frame containing the individuals whose
#'   ethnorace the user wants to impute. The following input variables are
#'   supported: `last_name`, `first_name`, `age`, `sex`, `party`, `multi_unit`,
#'   `state`, `county`, `zip`, `place`, `tract`, `district`, `block`.
#'
#' @param year The year for which Census data will be loaded. The function will
#'   retrieve the closest available Census data for the year.
#'
#' @param census_key Personal Census API key. See
#'   https://api.census.gov/data/key_signup.html
#'
#' @return A list with data objects necessary for
#'   \code{\link{impute_ethnorace}}.
#'
#' @export
load_bper_data <- function(input_data,
                           year,
                           census_key = NULL,
                           ll = list(
                             "last_name" = 1,
                             "first_name" = 1,
                             "state" = 1,
                             "county" = 1,
                             "zip" = 1,
                             "place" = 1,
                             "tract" = 1,
                             "district" = 1,
                             "block" = 1),
                           ...) {
  if (!is.null(census_key)) {
    Sys.setenv(CENSUS_KEY = census_key)
  }

  input_vars <- c()
  if ("last_name" %in% colnames(input_data)) {
    last_name <- load_surnames_data(year = year,
                                     psuedocount = ll$last_name)
    input_vars <- c(input_vars, "last")
  }
  if ("first_name" %in% colnames(input_data)) {
    first_name <- load_first_names_data(psuedocount = ll$first_name)
    input_vars <- c(input_vars, "first")
  }
  if ("party" %in% colnames(input_data)) {
    party <- load_parties_data(year = year)
    input_vars <- c(input_vars, "party")
  }
  if ("multi_unit" %in% colnames(input_data)) {
    multi_unit <- load_multi_unit_data(year = year)
    input_vars <- c(input_vars, "multi-unit")
  }
  if ("sex" %in% colnames(input_data)) {
    sex <- load_sex_age_data(year)$sexes
    input_vars <- c(input_vars, "sex")
  }
  if ("age" %in% colnames(input_data)) {
    age <- load_sex_age_data(year)$ages
    input_vars <- c(input_vars, "age")
  }

  if ("state" %in% colnames(input_data)) {
    input_states <- unique(input_data$state)
    state <-
      load_geo_data(geo_level = "state",
                    states = input_states,
                    year = year,
                    psuedocount = ll$state)
    if ("county" %in% colnames(input_data)) {
      county <-
        load_geo_data(geo_level = "county",
                      states = input_states,
                      year = year,
                      psuedocount = ll$county)
    }
    if ("zip" %in% colnames(input_data)) {
      zip <-
        load_geo_data(geo_level = "zip",
                      states = input_states,
                      year = year,
                      psuedocount = ll$zip)
    }
    if ("place" %in% colnames(input_data)) {
      place <-
        load_geo_data(geo_level = "place",
                      states = input_states,
                      year = year,
                      psuedocount = ll$place)
    }
    if ("tract" %in% colnames(input_data)) {
      tract <-
        load_geo_data(geo_level = "tract",
                      states = input_states,
                      year = year,
                      psuedocount = ll$tract)
    }
    if ("district" %in% colnames(input_data)) {
      district <-
        load_geo_data(geo_level = "congressional district",
                      states = input_states,
                      year = year,
                      psuedocount = ll$district)
    }
    if ("block" %in% colnames(input_data)) {
      block <-
        load_geo_data(geo_level = "block",
                      state = input_states,
                      year = year,
                      psuedocount = ll$block)
    }
    input_vars <- c(input_vars, "geo")
  }

  # Put all loaded input data objects into one list
  bper_data <- as.list(environment())
  # Remove the original input data set to save memory
  bper_data$input_data <- NULL

  return(bper_data)
}



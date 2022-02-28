
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
                           census_key = NULL) {
  if (!is.null(census_key)) {
    Sys.setenv(CENSUS_KEY = census_key)
  }

  input_vars <- c()
  if ("last_name" %in% colnames(input_data)) {
    print("Loading last names data")
    last_name <- load_surnames_data(year = year)
    input_vars <- c(input_vars, "last")
  }
  if ("first_name" %in% colnames(input_data)) {
    print("Loading first names data")
    first_name <- load_first_names_data()
    input_vars <- c(input_vars, "first")
  }
  if ("party" %in% colnames(input_data)) {
    print("Loading political party data")
    party <- load_parties_data(year = year)
    input_vars <- c(input_vars, "party")
  }
  if ("multi_unit" %in% colnames(input_data)) {
    print("Loading multi-unit data")
    multi_unit <- load_multi_unit_data(year = year)
    input_vars <- c(input_vars, "multi-unit")
  }
  if ("sex" %in% colnames(input_data) &
      "age" %in% colnames(input_data)) {
    print("Loading sex and age data")
    sex_age <- load_sex_age_data(year)
    sex <- sex_ages$sexes
    age <- sex_ages$ages
    input_vars <- c(input_vars, "sex", "age")
  } else if ("sex" %in% colnames(input_data)) {
    print("Loading sex data")
    sex <- load_sex_age_data(year)$sexes
    input_vars <- c(input_vars, "sex")
  } else if ("age" %in% colnames(input_data)) {
    print("Loading age data")
    age <- load_sex_age_data(year)$ages
    input_vars <- c(input_vars, "age")
  }

  if ("state" %in% colnames(input_data)) {
    print("Loading state data")
    input_states <- unique(input_data$state)
    state <-
      load_geo_data(geo_level = "state",
                    states = input_states,
                    year = year)
    if ("county" %in% colnames(input_data)) {
      print("Loading county data")
      county <-
        load_geo_data(geo_level = "county",
                      states = input_states,
                      year = year)
    }
    if ("zip" %in% colnames(input_data)) {
      print("Loading ZIP data")
      zip <-
        load_geo_data(geo_level = "zip",
                      states = input_states,
                      year = year)
    }
    if ("place" %in% colnames(input_data)) {
      print("Loading place data")
      place <-
        load_geo_data(geo_level = "place",
                      states = input_states,
                      year = year)
    }
    if ("tract" %in% colnames(input_data)) {
      print("Loading tract data")
      tract <-
        load_geo_data(geo_level = "tract",
                      states = input_states,
                      year = year)
    }
    if ("district" %in% colnames(input_data)) {
      print("Loading district data")
      district <-
        load_geo_data(geo_level = "congressional district",
                      states = input_states,
                      year = year)
    }
    if ("block" %in% colnames(input_data)) {
      print("Loading block data")
      block <-
        load_geo_data(geo_level = "block",
                      state = input_states,
                      year = year)
    }
    input_vars <- c(input_vars, "geo")
  }

  # Put all loaded input data objects into one list
  bper_data <- as.list(environment())
  # Remove the original input data set to save memory
  bper_data$input_data <- NULL

  return(bper_data)
}



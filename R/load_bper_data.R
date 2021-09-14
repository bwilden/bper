
load_bper_data <- function(input_df, year) {
  input_vars <- c()
  if ("last_name" %in% colnames(input_df)) {
    last_names <- load_surnames_data(year = year)
    input_vars <- c(input_vars, "last")
  }
  if ("first_name" %in% colnames(input_df)) {
    first_names <- load_first_names_data()
    input_vars <- c(input_vars, "first")
  }
  if ("party" %in% colnames(input_df)) {
    parties <- load_parties_data(year = year)
    input_vars <- c(input_vars, "party")
  }
  if ("multi_unit" %in% colnames(input_df)) {
    multi_units <- load_multi_unit_data(year = year)
    input_vars <- c(input_vars, "multi-unit")
  }

  if ("sex" %in% colnames(input_df) &
      "age" %in% colnames(input_df)) {
    sex_ages <- load_sex_age_data(year)$sex_ages
    input_vars <- c(input_vars, "sex-age")
  } else if ("sex" %in% colnames(input_df)) {
    sexes <- load_sex_age_data(year)$sexes
    input_vars <- c(input_vars, "sex")
  } else if ("age" %in% colnames(input_df)) {
    ages <- load_sex_age_data(year)$ages
    input_vars <- c(input_vars, "age")
  }

  if ("state" %in% colnames(input_df)) {
    input_states <- unique(input_df$state)
    states <-
      load_geo_data(geo_level = "state",
                    states = input_states,
                    year = year)
    if ("county" %in% colnames(input_df)) {
      counties <-
        load_geo_data(geo_level = "county",
                      states = input_states,
                      year = year)
    }
    if ("zip" %in% colnames(input_df)) {
      if (year < 2011) {
        closest_year <- 2011
      }
      zips <-
        load_geo_data(geo_level = "zip",
                      states = input_states,
                      year = closest_year)
    }
    if ("place" %in% colnames(input_df)) {
      places <-
        load_geo_data(geo_level = "place",
                      states = input_states,
                      year = year)
    }
    if ("tract" %in% colnames(input_df)) {
      tracts <-
        load_geo_data(geo_level = "tract",
                      states = input_states,
                      year = year)
    }
    if ("district" %in% colnames(input_df)) {
      districts <-
        load_geo_data(geo_level = "congressional district",
                      states = input_states,
                      year = year)
    }
    if ("block" %in% colnames(input_df)) {
      data_years <- c(1990, 2000, 2010)
      closest_year <- data_years[which.min(abs(data_years - year))]
      blocks <-
        load_geo_data(geo_level = "block",
                      state = input_states,
                      year = closest_year)
    }
    input_vars <- c(input_vars, "geo")
  }

  # Put all loaded input data objects into one list
  bper_data <- as.list(environment())
  # Remove the original input data set to save memory
  bper_data[length(bper_data)] <- NULL

  return(bper_data)
}



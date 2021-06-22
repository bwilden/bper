
load_bper_data <- function(input_df) {
  sex_ages_data <- load_sex_age_data()
  input_states <- unique(input_df$state)
  bper_data <- list(
    last_names = load_surnames_data(),
    first_names = load_first_names_data(),
    parties = load_parties_data(),
    multi_units = load_multi_unit_data(),
    sex_ages = sex_ages_data$sex_ages,
    sexes = sex_ages_data$sexes,
    ages = sex_ages_data$ages,
    counties = load_acs5_data(geo_level = "county", states = input_states)
  )
  return(bper_data)
}



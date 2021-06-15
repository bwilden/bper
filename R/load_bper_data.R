
load_bper_data <- function(geo) {

  bper_data <- list(
    last_names = load_surnames_data(),
    first_names = load_first_names_data(),
    parties = load_parties_data(),
    multi_units = load_multi_unit_data(),
    sex_ages = load_sex_age_data(),
    geos = load_geo_data(geo = geo)
  )
  return(bper_data)
}

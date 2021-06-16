


bper_naive_bayes <- function(data,
                             priors_set = c("last", "first", "party", "multi-unit", "sex-age", "geo")) {

  for (prior in priors_set) {
    data <- data %>%
      rowwise() %>%
      mutate(norm_factor = !!sym(paste0("pr_aian|", prior)) *
               prod(c_across(ends_with("aian") & !contains(prior))) +
               !!sym(paste0("pr_api|", prior)) *
               prod(c_across(ends_with("api") & !contains(prior))) +
               !!sym(paste0("pr_black|", prior)) *
               prod(c_across(ends_with("black") & !contains(prior))) +
               !!sym(paste0("pr_hispanic|", prior)) *
               prod(c_across(ends_with("hispanic") & !contains(prior))) +
               !!sym(paste0("pr_other|", prior)) *
               prod(c_across(ends_with("other") & !contains(prior))) +
               !!sym(paste0("pr_white|", prior)) *
               prod(c_across(ends_with("white") & !contains(prior))),
             "pp_aian_{prior}" := !!sym(paste0("pr_aian|", prior)) *
               prod(c_across(ends_with("|aian") & !contains(prior))) / norm_factor,
             "pp_api_{prior}" :=  !!sym(paste0("pr_api|", prior)) *
               prod(c_across(ends_with("|api") & !contains(prior))) / norm_factor,
             "pp_black_{prior}" := !!sym(paste0("pr_black|", prior)) *
               prod(c_across(ends_with("|black") & !contains(prior))) / norm_factor,
             "pp_hispanic_{prior}" := !!sym(paste0("pr_hispanic|", prior)) *
               prod(c_across(ends_with("|hispanic") & !contains(prior))) / norm_factor,
             "pp_other_{prior}" := !!sym(paste0("pr_other|", prior)) *
               prod(c_across(ends_with("|other") & !contains(prior))) / norm_factor,
             "pp_white_{prior}" := !!sym(paste0("pr_white|", prior)) *
               prod(c_across(ends_with("|white") & !contains(prior))) / norm_factor) %>%
      select(-norm_factor) %>%
      ungroup()
  }

  data <- data %>%
    mutate(pred_aian = rowMeans(across(contains("pp_aian"))),
           pred_api = rowMeans(across(contains("pp_api"))),
           pred_black = rowMeans(across(contains("pp_black"))),
           pred_hispanic = rowMeans(across(contains("pp_hispanic"))),
           pred_other = rowMeans(across(contains("pp_other"))),
           pred_white = rowMeans(across(contains("pp_white")))) %>%
    rowwise() %>%
    mutate(pred_race = purrr::pmap(across(contains("pred")),
                                   ~ names(c(...)[which.max(c(...))]))) %>%
    ungroup() %>%
    mutate(pred_race = gsub("pred_", "", pred_race))

  return(data)
}


#' Predict Ethnicity/Race
#'
#' Calculates posterior probabilities for individual ethnorace categories using
#' the Naive Bayes algorithm. Also returns highest predicted race as a new
#' string column in the data frame.
#'
#' @param data
#'
#' @return Returns the original data.frame with the additional columns for
#'   ethnorace probabilities and predicted category.
#'
#' @export
predict_ethnorace <- function(data, bper_data = NULL, geo) {

# Prep Data/Correct Input Errors ------------------------------------------

  original_columns <- colnames(data)

  predictions <- data %>% mutate(id = row_number()) %>%
    left_join(state_codes)

  if (is.null(bper_data)) {
    last_names <- load_surnames_data()
    first_names <- load_first_names_data()
    parties <- load_parties_data()
    multi_units <- load_multi_unit_data()
    sex_ages <- load_sex_age_data()
    geos <- load_geo_data(geo = geo)
  } else {
    last_names <- bper_data$last_names
    first_names <- bper_data$first_names
    parties <- bper_data$parties
    multi_units <- bper_data$multi_units
    sex_ages <- bper_data$sex_ages
    geos <- bper_data$geo
  }

  predictions <- predictions %>%
    left_join(last_names) %>%
    left_join(first_names) %>%
    left_join(parties) %>%
    left_join(multi_units) %>%
    left_join(sex_ages, by = c("sex", "age")) %>%
    left_join(geos)

  predictions <- bper_naive_bayes(predictions)

  predictions <- predictions %>%
    select(all_of(original_columns), contains("pred"))

  return(predictions)
}








# mutate(
# norm_factor_s = ((pr_black_s * pr_f_black * pr_g_black * pr_p_black * pr_a_black * pr_fem_black * pr_y_black) +
#   (pr_white_s * pr_f_white * pr_g_white * pr_p_white * pr_a_white * pr_fem_white * pr_y_white) +
#   (pr_hispanic_s * pr_f_hispanic * pr_g_hispanic * pr_p_hispanic * pr_a_hispanic * pr_fem_hispanic * pr_y_hispanic) +
#   (pr_api_s * pr_f_api * pr_g_api * pr_p_api * pr_a_api * pr_fem_api * pr_y_api) +
#   (pr_aian_s * pr_f_aian * pr_g_aian * pr_p_aian * pr_a_aian * pr_fem_aian * pr_y_aian) +
#   (pr_other_s * pr_f_other * pr_g_other * pr_p_other * pr_a_other * pr_fem_other * pr_y_other)
# ),
# prob_black_s = pr_black_s *
#   (pr_f_black * pr_g_black * pr_p_black * pr_a_black * pr_fem_black * pr_y_black) / norm_factor_s,
#         prob_white_s = pr_white_s *
#           (pr_f_white * pr_g_white * pr_p_white * pr_a_white * pr_fem_white * pr_y_white) / norm_factor_s,
#         prob_hispanic_s = pr_hispanic_s *
#           (pr_f_hispanic * pr_g_hispanic * pr_p_hispanic * pr_a_hispanic * pr_fem_hispanic * pr_y_hispanic) / norm_factor_s,
#         prob_api_s = pr_api_s *
#           (pr_f_api * pr_g_api * pr_p_api * pr_a_api * pr_fem_api * pr_y_api) / norm_factor_s,
#         prob_aian_s = pr_aian_s *
#           (pr_f_aian * pr_g_aian * pr_p_aian * pr_a_aian * pr_fem_aian * pr_y_aian) / norm_factor_s,
#         prob_other_s = pr_other_s *
#           (pr_f_other * pr_g_other * pr_p_other * pr_a_other * pr_fem_other * pr_y_other) / norm_factor_s
















# Merge Geolocation Probabilities -----------------------------------------

  # Split data into sets depending on which geolocation variable exists

#   if (exists("blocks")) {
#     block_matches <- df %>%
#       filter(!is.na(block)) %>%
#       left_join(blocks, by = "block")
#   } else {
#     block_matches <- data.frame(id = NA)
#   }
#
#   zip_matches <- df %>%
#     filter(!is.na(zip), id %notin% block_matches$id) %>%
#     left_join(zips, by = "zip")
#
#   place_matches <- df %>%
#     filter(!is.na(place), id %notin% c(block_matches$id, zip_matches$id)) %>%
#     left_join(places, by = "place")
#
#   county_matches <- df %>%
#     filter(!is.na(county), id %notin% c(block_matches$id, zip_matches$id, place_matches$id)) %>%
#     left_join(counties, by = "county")
#
#   state_matches <- df %>%
#     filter(!is.na(state), id %notin% c(block_matches$id, zip_matches$id, place_matches$id, county_matches$id)) %>%
#     left_join(states, by = "state")
#
#   none_matches <- df %>%
#     filter(id %notin% c(state_matches$id, block_matches$id, zip_matches$id, place_matches$id, county_matches$id)) %>%
#     mutate(GEOID = "nationwide") %>%
#     left_join(nationwide, by = "GEOID") %>%
#     select(-GEOID)
#
#   df <- rbind(zip_matches, place_matches, county_matches, state_matches, none_matches)
#
#   if (exists("blocks")) {
#     df <- rbind(df, block_matches)
#   }
#
#
# # Merge Other Probabilities -----------------------------------------------
#
#   # Add "alt" names to merge with "all other" first/last name probabilities
#   df <- df %>%
#     mutate(
#       first_name_alt = ifelse(first_name %notin% firstnames$first_name,
#         "ALL OTHER FIRST NAMES", first_name
#       ),
#       last_name_alt = ifelse(last_name %notin% surnames$last_name,
#         "ALL OTHER NAMES", last_name
#       )
#     ) %>%
#     left_join(surnames, by = c("last_name_alt" = "last_name")) %>%
#     left_join(firstnames, by = c("first_name_alt" = "first_name")) %>%
#     # Remove alternative names
#     select(-c(first_name_alt, last_name_alt)) %>%
#
#     left_join(parties, by = "party") %>%
#     left_join(apartments, by = "apartment") %>%
#     left_join(genders, by = "female") %>%
#     left_join(birth_years, by = "birth_year")
#
#
# # Compute Posterior Probabilities -----------------------------------------
#
#   df <- df %>%
#     # Input pr = 1 for unmatched priors/likelihoods
#     mutate_at(vars(contains("pr_")), ~ ifelse(is.na(.), 1, .)) %>%
#     # Doing all the math...
#     mutate(
#       norm_factor_s = ((pr_black_s * pr_f_black * pr_g_black * pr_p_black * pr_a_black * pr_fem_black * pr_y_black) +
#         (pr_white_s * pr_f_white * pr_g_white * pr_p_white * pr_a_white * pr_fem_white * pr_y_white) +
#         (pr_hispanic_s * pr_f_hispanic * pr_g_hispanic * pr_p_hispanic * pr_a_hispanic * pr_fem_hispanic * pr_y_hispanic) +
#         (pr_api_s * pr_f_api * pr_g_api * pr_p_api * pr_a_api * pr_fem_api * pr_y_api) +
#         (pr_aian_s * pr_f_aian * pr_g_aian * pr_p_aian * pr_a_aian * pr_fem_aian * pr_y_aian) +
#         (pr_other_s * pr_f_other * pr_g_other * pr_p_other * pr_a_other * pr_fem_other * pr_y_other)
#       ),
#       prob_black_s = pr_black_s *
#         (pr_f_black * pr_g_black * pr_p_black * pr_a_black * pr_fem_black * pr_y_black) / norm_factor_s,
#       prob_white_s = pr_white_s *
#         (pr_f_white * pr_g_white * pr_p_white * pr_a_white * pr_fem_white * pr_y_white) / norm_factor_s,
#       prob_hispanic_s = pr_hispanic_s *
#         (pr_f_hispanic * pr_g_hispanic * pr_p_hispanic * pr_a_hispanic * pr_fem_hispanic * pr_y_hispanic) / norm_factor_s,
#       prob_api_s = pr_api_s *
#         (pr_f_api * pr_g_api * pr_p_api * pr_a_api * pr_fem_api * pr_y_api) / norm_factor_s,
#       prob_aian_s = pr_aian_s *
#         (pr_f_aian * pr_g_aian * pr_p_aian * pr_a_aian * pr_fem_aian * pr_y_aian) / norm_factor_s,
#       prob_other_s = pr_other_s *
#         (pr_f_other * pr_g_other * pr_p_other * pr_a_other * pr_fem_other * pr_y_other) / norm_factor_s,
#       norm_factor_f = ((pr_black_f * pr_s_black * pr_g_black * pr_p_black * pr_a_black * pr_fem_black * pr_y_black) +
#         (pr_white_f * pr_s_white * pr_g_white * pr_p_white * pr_a_white * pr_fem_white * pr_y_white) +
#         (pr_hispanic_f * pr_s_hispanic * pr_g_hispanic * pr_p_hispanic * pr_a_hispanic * pr_fem_hispanic * pr_y_hispanic) +
#         (pr_api_f * pr_s_api * pr_g_api * pr_p_api * pr_a_api * pr_fem_api * pr_y_api) +
#         (pr_aian_f * pr_s_aian * pr_g_aian * pr_p_aian * pr_a_aian * pr_fem_aian * pr_y_aian) +
#         (pr_other_f * pr_s_other * pr_g_other * pr_p_other * pr_a_other * pr_fem_other * pr_y_other)
#       ),
#       prob_black_f = pr_black_f *
#         (pr_s_black * pr_g_black * pr_p_black * pr_a_black * pr_fem_black * pr_y_black) / norm_factor_f,
#       prob_white_f = pr_white_f *
#         (pr_s_white * pr_g_white * pr_p_white * pr_a_white * pr_fem_white * pr_y_white) / norm_factor_f,
#       prob_hispanic_f = pr_hispanic_f *
#         (pr_s_hispanic * pr_g_hispanic * pr_p_hispanic * pr_a_hispanic * pr_fem_hispanic * pr_y_hispanic) / norm_factor_f,
#       prob_api_f = pr_api_f *
#         (pr_s_api * pr_g_api * pr_p_api * pr_a_api * pr_fem_api * pr_y_api) / norm_factor_f,
#       prob_aian_f = pr_aian_f *
#         (pr_s_aian * pr_g_aian * pr_p_aian * pr_a_aian * pr_fem_aian * pr_y_aian) / norm_factor_f,
#       prob_other_f = pr_other_f *
#         (pr_s_other * pr_g_other * pr_p_other * pr_a_other * pr_fem_other * pr_y_other) / norm_factor_f,
#       norm_factor_g = ((pr_black_g * pr_f_black * pr_s_black * pr_p_black * pr_a_black * pr_fem_black * pr_y_black) +
#         (pr_white_g * pr_f_white * pr_s_white * pr_p_white * pr_a_white * pr_fem_white * pr_y_white) +
#         (pr_hispanic_g * pr_f_hispanic * pr_s_hispanic * pr_p_hispanic * pr_a_hispanic * pr_fem_hispanic * pr_y_hispanic) +
#         (pr_api_g * pr_f_api * pr_s_api * pr_p_api * pr_a_api * pr_fem_api * pr_y_api) +
#         (pr_aian_g * pr_f_aian * pr_s_aian * pr_p_aian * pr_a_aian * pr_fem_aian * pr_y_aian) +
#         (pr_other_g * pr_f_other * pr_s_other * pr_p_other * pr_a_other * pr_fem_other * pr_y_other)
#       ),
#       prob_black_g = pr_black_g *
#         (pr_f_black * pr_s_black * pr_p_black * pr_a_black * pr_fem_black * pr_y_black) / norm_factor_g,
#       prob_white_g = pr_white_g *
#         (pr_f_white * pr_s_white * pr_p_white * pr_a_white * pr_fem_white * pr_y_white) / norm_factor_g,
#       prob_hispanic_g = pr_hispanic_g *
#         (pr_f_hispanic * pr_s_hispanic * pr_p_hispanic * pr_a_hispanic * pr_fem_hispanic * pr_y_hispanic) / norm_factor_g,
#       prob_api_g = pr_api_g *
#         (pr_f_api * pr_s_api * pr_p_api * pr_a_api * pr_fem_api * pr_y_api) / norm_factor_g,
#       prob_aian_g = pr_aian_g *
#         (pr_f_aian * pr_s_aian * pr_p_aian * pr_a_aian * pr_fem_aian * pr_y_aian) / norm_factor_g,
#       prob_other_g = pr_other_g *
#         (pr_f_other * pr_s_other * pr_p_other * pr_a_other * pr_fem_other * pr_y_other) / norm_factor_g,
#       norm_factor_p = ((pr_black_p * pr_f_black * pr_g_black * pr_s_black * pr_a_black * pr_fem_black * pr_y_black) +
#         (pr_white_p * pr_f_white * pr_g_white * pr_s_white * pr_a_white * pr_fem_white * pr_y_white) +
#         (pr_hispanic_p * pr_f_hispanic * pr_g_hispanic * pr_s_hispanic * pr_a_hispanic * pr_fem_hispanic * pr_y_hispanic) +
#         (pr_api_p * pr_f_api * pr_g_api * pr_s_api * pr_a_api * pr_fem_api * pr_y_api) +
#         (pr_aian_p * pr_f_aian * pr_g_aian * pr_s_aian * pr_a_aian * pr_fem_aian * pr_y_aian) +
#         (pr_other_p * pr_f_other * pr_g_other * pr_s_other * pr_a_other * pr_fem_other * pr_y_other)
#       ),
#       prob_black_p = pr_black_p *
#         (pr_f_black * pr_g_black * pr_s_black * pr_a_black * pr_fem_black * pr_y_black) / norm_factor_p,
#       prob_white_p = pr_white_p *
#         (pr_f_white * pr_g_white * pr_s_white * pr_a_white * pr_fem_white * pr_y_white) / norm_factor_p,
#       prob_hispanic_p = pr_hispanic_p *
#         (pr_f_hispanic * pr_g_hispanic * pr_s_hispanic * pr_a_hispanic * pr_fem_hispanic * pr_y_hispanic) / norm_factor_p,
#       prob_api_p = pr_api_p *
#         (pr_f_api * pr_g_api * pr_s_api * pr_a_api * pr_fem_api * pr_y_api) / norm_factor_p,
#       prob_aian_p = pr_aian_p *
#         (pr_f_aian * pr_g_aian * pr_s_aian * pr_a_aian * pr_fem_aian * pr_y_aian) / norm_factor_p,
#       prob_other_p = pr_other_p *
#         (pr_f_other * pr_g_other * pr_s_other * pr_a_other * pr_fem_other * pr_y_other) / norm_factor_p,
#       norm_factor_a = ((pr_black_a * pr_f_black * pr_g_black * pr_p_black * pr_s_black * pr_fem_black * pr_y_black) +
#         (pr_white_a * pr_f_white * pr_g_white * pr_p_white * pr_s_white * pr_fem_white * pr_y_white) +
#         (pr_hispanic_a * pr_f_hispanic * pr_g_hispanic * pr_p_hispanic * pr_s_hispanic * pr_fem_hispanic * pr_y_hispanic) +
#         (pr_api_a * pr_f_api * pr_g_api * pr_p_api * pr_s_api * pr_fem_api * pr_y_api) +
#         (pr_aian_a * pr_f_aian * pr_g_aian * pr_p_aian * pr_s_aian * pr_fem_aian * pr_y_aian) +
#         (pr_other_a * pr_f_other * pr_g_other * pr_p_other * pr_s_other * pr_fem_other * pr_y_other)
#       ),
#       prob_black_a = pr_black_a *
#         (pr_f_black * pr_g_black * pr_p_black * pr_s_black * pr_fem_black * pr_y_black) / norm_factor_a,
#       prob_white_a = pr_white_a *
#         (pr_f_white * pr_g_white * pr_p_white * pr_s_white * pr_fem_white * pr_y_white) / norm_factor_a,
#       prob_hispanic_a = pr_hispanic_a *
#         (pr_f_hispanic * pr_g_hispanic * pr_p_hispanic * pr_s_hispanic * pr_fem_hispanic * pr_y_hispanic) / norm_factor_a,
#       prob_api_a = pr_api_a *
#         (pr_f_api * pr_g_api * pr_p_api * pr_s_api * pr_fem_api * pr_y_api) / norm_factor_a,
#       prob_aian_a = pr_aian_a *
#         (pr_f_aian * pr_g_aian * pr_p_aian * pr_s_aian * pr_fem_aian * pr_y_aian) / norm_factor_a,
#       prob_other_a = pr_other_a *
#         (pr_f_other * pr_g_other * pr_p_other * pr_s_other * pr_fem_other * pr_y_other) / norm_factor_a,
#       norm_factor_fem = ((pr_black_fem * pr_f_black * pr_g_black * pr_p_black * pr_a_black * pr_s_black * pr_y_black) +
#         (pr_white_fem * pr_f_white * pr_g_white * pr_p_white * pr_a_white * pr_s_white * pr_y_white) +
#         (pr_hispanic_fem * pr_f_hispanic * pr_g_hispanic * pr_p_hispanic * pr_a_hispanic * pr_s_hispanic * pr_y_hispanic) +
#         (pr_api_fem * pr_f_api * pr_g_api * pr_p_api * pr_a_api * pr_s_api * pr_y_api) +
#         (pr_aian_fem * pr_f_aian * pr_g_aian * pr_p_aian * pr_a_aian * pr_s_aian * pr_y_aian) +
#         (pr_other_fem * pr_f_other * pr_g_other * pr_p_other * pr_a_other * pr_s_other * pr_y_other)
#       ),
#       prob_black_fem = pr_black_fem *
#         (pr_f_black * pr_g_black * pr_p_black * pr_a_black * pr_s_black * pr_y_black) / norm_factor_fem,
#       prob_white_fem = pr_white_fem *
#         (pr_f_white * pr_g_white * pr_p_white * pr_a_white * pr_s_white * pr_y_white) / norm_factor_fem,
#       prob_hispanic_fem = pr_hispanic_fem *
#         (pr_f_hispanic * pr_g_hispanic * pr_p_hispanic * pr_a_hispanic * pr_s_hispanic * pr_y_hispanic) / norm_factor_fem,
#       prob_api_fem = pr_api_fem *
#         (pr_f_api * pr_g_api * pr_p_api * pr_a_api * pr_s_api * pr_y_api) / norm_factor_fem,
#       prob_aian_fem = pr_aian_fem *
#         (pr_f_aian * pr_g_aian * pr_p_aian * pr_a_aian * pr_s_aian * pr_y_aian) / norm_factor_fem,
#       prob_other_fem = pr_other_fem *
#         (pr_f_other * pr_g_other * pr_p_other * pr_a_other * pr_s_other * pr_y_other) / norm_factor_fem,
#       norm_factor_y = ((pr_black_y * pr_f_black * pr_g_black * pr_p_black * pr_a_black * pr_fem_black * pr_s_black) +
#         (pr_white_y * pr_f_white * pr_g_white * pr_p_white * pr_a_white * pr_fem_white * pr_s_white) +
#         (pr_hispanic_y * pr_f_hispanic * pr_g_hispanic * pr_p_hispanic * pr_a_hispanic * pr_fem_hispanic * pr_s_hispanic) +
#         (pr_api_y * pr_f_api * pr_g_api * pr_p_api * pr_a_api * pr_fem_api * pr_s_api) +
#         (pr_aian_y * pr_f_aian * pr_g_aian * pr_p_aian * pr_a_aian * pr_fem_aian * pr_s_aian) +
#         (pr_other_y * pr_f_other * pr_g_other * pr_p_other * pr_a_other * pr_fem_other * pr_s_other)
#       ),
#       prob_black_y = pr_black_y *
#         (pr_f_black * pr_g_black * pr_p_black * pr_a_black * pr_fem_black * pr_s_black) / norm_factor_y,
#       prob_white_y = pr_white_y *
#         (pr_f_white * pr_g_white * pr_p_white * pr_a_white * pr_fem_white * pr_s_white) / norm_factor_y,
#       prob_hispanic_y = pr_hispanic_y *
#         (pr_f_hispanic * pr_g_hispanic * pr_p_hispanic * pr_a_hispanic * pr_fem_hispanic * pr_s_hispanic) / norm_factor_y,
#       prob_api_y = pr_api_y *
#         (pr_f_api * pr_g_api * pr_p_api * pr_a_api * pr_fem_api * pr_s_api) / norm_factor_y,
#       prob_aian_y = pr_aian_y *
#         (pr_f_aian * pr_g_aian * pr_p_aian * pr_a_aian * pr_fem_aian * pr_s_aian) / norm_factor_y,
#       prob_other_y = pr_other_y *
#         (pr_f_other * pr_g_other * pr_p_other * pr_a_other * pr_fem_other * pr_s_other) / norm_factor_y
#     )
#
#   # Calculates means of all choices of "prior"
#   df <- df %>%
#     mutate(
#       prob_black = rowMeans(across(contains("prob_black"))),
#       prob_white = rowMeans(across(contains("prob_white"))),
#       prob_hispanic = rowMeans(across(contains("prob_hispanic"))),
#       prob_api = rowMeans(across(contains("prob_api"))),
#       prob_aian = rowMeans(across(contains("prob_aian"))),
#       prob_other = rowMeans(across(contains("prob_other")))
#     )
#
#   # Calculate highest posterior probability and impute race/ethnicity label
#   df <- arg_max_cols(df, 6, max_col_name = "pred_race") %>%
#     mutate(pred_race = gsub("prob_", "", pred_race))
#
#   return(df)
# }

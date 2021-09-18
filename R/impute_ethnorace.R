
#' Impute Ethnicity/Race
#'
#' Calculates posterior probabilities for individual ethnorace categories using
#' the Naive Bayes algorithm. Also returns highest probability ethnorace as a
#' new character column in the data frame.
#'
#' @param input_data The input data frame containing the individuals whose
#'   ethnorace the user wants to impute. The following input variables are
#'   supported: `last_name`, `first_name`, `age`, `sex`, `party`, `multi_unit`,
#'   `state`, `county`, `zip`, `place`, `tract`, `district`, `block`.
#'
#' @param bper_data The data list containing ethnorace conditional
#'   probabilities. If left empty, will default to downloading directly from
#'   Census API. Use the function \code{\link{load_bper_data}} to save this data
#'   list ahead of time.
#'
#' @param year The year for which Census data will be loaded. The function will
#'   retrieve the closest available Census data for the year.
#'
#' @param census_key Personal Census API key. See
#'   https://api.census.gov/data/key_signup.html
#'
#' @return The original data frame with the additional columns for ethnorace
#'   probabilities and highest probability imputed category.
#'
#' @export
impute_ethnorace <- function(input_data,
                             bper_data = NULL,
                             year,
                             census_key = "5e4c2b8438222753a7f4753fa78855eca73b9950") {

  if (is.null(bper_data)) {
    bper_data <- load_bper_data(input_data = input_data,
                                year = year,
                                census_key = census_key)
  }

  original_columns <- colnames(input_data)

  input_data <- left_join(input_data, state_codes)

  # Merge in input data that match columns in original data
  for (data_set in names(bper_data)) {
    if (data_set %in% bper_vars) {
      input_data <- left_join(input_data, bper_data[[data_set]])
    }
  }

  # Fill in missing geography columns
  for (geo_level in bper_geos) {
    if (geo_level %notin% original_columns) {
      for (ethnorace in ethnorace_set) {
        input_data <- input_data %>%
          mutate("pr_{ethnorace}|{geo_level}" := NA_character_,
                 "pr_{geo_level}|{ethnorace}" := NA_character_)
      }
    }
  }

  # Select geography conditional probabilities
  # at finest level of geography available per individual
  for (ethnorace in ethnorace_set) {
    input_data <- input_data %>%
      mutate("pr_{ethnorace}|geo" :=
               ifelse(!is.na(!!sym(paste0("pr_", ethnorace, "|block"))),
                      !!sym(paste0("pr_", ethnorace, "|block")),
               ifelse(!is.na(!!sym(paste0("pr_", ethnorace, "|tract"))),
                      !!sym(paste0("pr_", ethnorace, "|tract")),
               ifelse(!is.na(!!sym(paste0("pr_", ethnorace, "|zip"))),
                      !!sym(paste0("pr_", ethnorace, "|zip")),
               ifelse(!is.na(!!sym(paste0("pr_", ethnorace, "|place"))),
                      !!sym(paste0("pr_", ethnorace, "|place")),
               ifelse(!is.na(!!sym(paste0("pr_", ethnorace, "|district"))),
                      !!sym(paste0("pr_", ethnorace, "|district")),
               ifelse(!is.na(!!sym(paste0("pr_", ethnorace, "|county"))),
                      !!sym(paste0("pr_", ethnorace, "|county")),
               ifelse(!is.na(!!sym(paste0("pr_", ethnorace, "|state"))),
                      !!sym(paste0("pr_", ethnorace, "|state")),
               NA_real_))))))),
            "pr_geo|{ethnorace}" :=
              ifelse(!is.na(!!sym(paste0("pr_block|", ethnorace))),
                     !!sym(paste0("pr_block|", ethnorace)),
               ifelse(!is.na(!!sym(paste0("pr_tract|", ethnorace))),
                      !!sym(paste0("pr_tract|", ethnorace)),
               ifelse(!is.na(!!sym(paste0("pr_zip|", ethnorace))),
                      !!sym(paste0("pr_zip|", ethnorace)),
               ifelse(!is.na(!!sym(paste0("pr_place|", ethnorace))),
                      !!sym(paste0("pr_place|", ethnorace)),
               ifelse(!is.na(!!sym(paste0("pr_district|", ethnorace))),
                      !!sym(paste0("pr_district|", ethnorace)),
               ifelse(!is.na(!!sym(paste0("pr_county|", ethnorace))),
                      !!sym(paste0("pr_county|", ethnorace)),
               ifelse(!is.na(!!sym(paste0("pr_state|", ethnorace))),
                      !!sym(paste0("pr_state|", ethnorace)),
               NA_real_))))))))
  }

  # Remove extraneous geography columns
  input_data <- input_data %>%
    select(all_of(original_columns), contains(bper_data$input_vars))

  # Perform ethnorace probability calculations
  input_data <- bper_naive_bayes(input_data, priors_set = bper_data$input_vars)


  input_data <- input_data %>%
     select(all_of(original_columns), contains("pred_"))

  return(input_data)
}


# Naive Bayes computation function
bper_naive_bayes <- function(data, priors_set) {
  for (prior in priors_set) {
    data <- data %>%
      rowwise() %>%
      mutate(
        norm_factor = !!sym(paste0("pr_aian|", prior)) *
          prod(c_across(ends_with("aian") &
                          !contains(prior)))+!!sym(paste0("pr_api|", prior)) *
          prod(c_across(ends_with("api") & !contains(prior)))+!!sym(paste0("pr_black|", prior)) *
          prod(c_across(
            ends_with("black") & !contains(prior)
          ))+!!sym(paste0("pr_hispanic|", prior)) *
          prod(c_across(
            ends_with("hispanic") & !contains(prior)
          ))+!!sym(paste0("pr_other|", prior)) *
          prod(c_across(
            ends_with("other") & !contains(prior)
          ))+!!sym(paste0("pr_white|", prior)) *
          prod(c_across(
            ends_with("white") & !contains(prior)
          )),
        "pp_aian_{prior}" := !!sym(paste0("pr_aian|", prior)) *
          prod(c_across(
            ends_with("|aian") & !contains(prior)
          )) / norm_factor,
        "pp_api_{prior}" :=  !!sym(paste0("pr_api|", prior)) *
          prod(c_across(ends_with("|api") &
                          !contains(prior))) / norm_factor,
        "pp_black_{prior}" := !!sym(paste0("pr_black|", prior)) *
          prod(c_across(
            ends_with("|black") & !contains(prior)
          )) / norm_factor,
        "pp_hispanic_{prior}" := !!sym(paste0("pr_hispanic|", prior)) *
          prod(c_across(
            ends_with("|hispanic") & !contains(prior)
          )) / norm_factor,
        "pp_other_{prior}" := !!sym(paste0("pr_other|", prior)) *
          prod(c_across(
            ends_with("|other") & !contains(prior)
          )) / norm_factor,
        "pp_white_{prior}" := !!sym(paste0("pr_white|", prior)) *
          prod(c_across(
            ends_with("|white") & !contains(prior)
          )) / norm_factor
      ) %>%
      select(-norm_factor) %>%
      ungroup()
  }

  data <- data %>%
    mutate(
      pred_aian = rowMeans(across(contains("pp_aian"))),
      pred_api = rowMeans(across(contains("pp_api"))),
      pred_black = rowMeans(across(contains("pp_black"))),
      pred_hispanic = rowMeans(across(contains("pp_hispanic"))),
      pred_other = rowMeans(across(contains("pp_other"))),
      pred_white = rowMeans(across(contains("pp_white")))
    ) %>%
    rowwise() %>%
    mutate(pred_race = purrr::pmap(across(contains("pred")),
                                   ~ names(c(...)[which.max(c(...))]))) %>%
    ungroup() %>%
    mutate(pred_race = gsub("pred_", "", pred_race)) %>%
    select(pred_race, everything())

  return(data)
}



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
    mutate(pred_race = gsub("pred_", "", pred_race))

  return(data)
}


#' Predict Ethnicity/Race
#'
#' Calculates posterior probabilities for individual ethnorace categories using
#' the Naive Bayes algorithm. Also returns highest predicted race as a new
#' string column in the data frame.
#'
#' @param input_data The input data frame containing the individuals whose ethnorace
#'   the user wants to predict.
#'
#' @param bper_data The data list containing ethnorace conditional
#'   probabilities. If left empty, will default to downloading directly from
#'   Census API. Use the function `load_bper_data` to save this data ahead of
#'   time.
#'
#' @return Returns the original data.frame with the additional columns for
#'   ethnorace probabilities and predicted category.
#'
#' @export
predict_ethnorace <- function(input_data = example_persons, bper_data = NULL) {
  if (is.null(bper_data)) {
    bper_data <- load_bper_data(input_data)
  }

  original_columns <- colnames(input_data)

  for (geo_level in bper_geos) {
    if (geo_level %notin% original_columns) {
      input_data <- mutate(input_data, "{geo_level}" := NA_character_)
    }
  }

  input_data <- left_join(input_data, state_codes)

  for (data_set in names(bper_data)) {
    if (data_set %in% bper_vars) {
      input_data <- left_join(input_data, bper_data[[data_set]])
    }
  }

  if ("state" %in% original_columns) {
    for (ethnorace in ethnorace_set) {
      input_data <- input_data %>%
        mutate("pr_{ethnorace}|geo" := case_when(
          is.na(!!sym(paste0("pr_", ethnorace, "|county"))) ~
            !!sym(paste0("pr_", ethnorace, "|state")),
          is.na(!!sym(paste0("pr_", ethnorace, "|tract"))) ~
            !!sym(paste0("pr_", ethnorace, "|county")),
          TRUE ~ !!sym(paste0("pr_", ethnorace, "|tract"))),
          "pr_geo|{ethnorace}" := case_when(
            is.na(!!sym(paste0("pr_county|", ethnorace))) ~
              !!sym(paste0("pr_state|", ethnorace)),
            is.na(!!sym(paste0("pr_tract|", ethnorace))) ~
              !!sym(paste0("pr_county|", ethnorace)),
            TRUE ~ !!sym(paste0("pr_tract|", ethnorace))))
    }
  }


  input_data <- input_data %>%
    select(all_of(original_columns), contains(bper_data$input_vars))

   input_data <- bper_naive_bayes(input_data, priors_set = bper_data$input_vars)

   input_data <- input_data %>%
     select(all_of(original_columns), contains("pred_"))

  return(input_data)
}



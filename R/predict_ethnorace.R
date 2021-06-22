

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
predict_ethnorace <- function(input_data = example_persons, bper_data = NULL, geo_level) {
  if (is.null(bper_data)) {
    bper_data <- load_bper_data(geo_level)
  }

  original_columns <- colnames(input_data)

  input_data <- input_data %>% mutate(id = row_number()) %>%
    left_join(state_codes)

  input_vars <- c()
  if ("last_name" %in% original_columns) {
    input_data <- left_join(input_data, bper_data$last_names)
    input_vars <- c(input_vars, "last")
  }
  if ("first_name" %in% original_columns) {
    input_data <- left_join(input_data, bper_data$first_names)
    input_vars <- c(input_vars, "first")
  }
  if ("party" %in% original_columns) {
    input_data <- left_join(input_data, bper_data$parties)
    input_vars <- c(input_vars, "party")
  }
  if ("multi_unit" %in% original_columns) {
    input_data <- left_join(input_data, bper_data$multi_units)
    input_vars <- c(input_vars, "multi-unit")
  }
  if ("county" %in% original_columns) {
    input_data <- left_join(input_data, bper_data$counties)
    input_vars <- c(input_vars, "geo")
  }
  if ("sex" %in% original_columns &
      "age" %in% original_columns) {
    input_data <- left_join(input_data, bper_data$sex_ages)
    input_vars <- c(input_vars, "sex-age")
  } else if ("sex" %in% original_columns) {
    input_data <- left_join(input_data, bper_data$sexes)
    input_vars <- c(input_vars, "sex")
  } else if ("age" %in% original_columns) {
    input_data <- left_join(input_data, bper_data$ages)
    input_vars <- c(input_vars, "age")
  }

  input_data <- bper_naive_bayes(input_data, priors_set = input_vars)

  output_data <- input_data %>%
    select(all_of(original_columns), contains("pred"))

  return(output_data)
}



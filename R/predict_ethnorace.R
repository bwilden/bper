

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
#' @param data The input data frame containing the individuals whose ethnorace
#'   the user wants to predict.
#'
#' @param bper_data The data list containing ethnorace conditional
#'   probabilities. If left empty, will default to downloading directly from
#'   Census API. Use the function `load_bper_data` to save this data ahead of
#'   time.
#'
#' @param geo Input geography used to make predictions.
#'
#' @return Returns the original data.frame with the additional columns for
#'   ethnorace probabilities and predicted category.
#'
#' @export
predict_ethnorace <- function(data, bper_data = NULL, geo) {
  original_columns <- colnames(data)

  data <- data %>% mutate(id = row_number()) %>%
    left_join(state_codes)

  input_set <- c()
  if (is.null(bper_data)) {
    if ("last_name" %in% original_columns) {
      data <- data %>% left_join(load_surnames_data())
      input_set <- c(input_set, "last")
    }
    if ("first_name" %in% original_columns) {
      data <- data %>% left_join(load_first_names_data())
      input_set <- c(input_set, "first")
    }
    if ("party" %in% original_columns) {
      data <- data %>% left_join(load_parties_data())
      input_set <- c(input_set, "party")
    }
    if ("multi_unit" %in% original_columns) {
      data <- data %>% left_join(load_multi_unit_data())
      input_set <- c(input_set, "multi-unit")
    }
    if ("sex" %in% original_columns
        & "age" %in% original_columns) {
      data <- data %>% left_join(load_sex_age_data()$sex_ages)
      input_set <- c(input_set, "sex-age")
    } else if ("sex" %in% original_columns) {
      data <- data %>% left_join(load_sex_age_data()$sexes)
      input_set <- c(input_set, "sex")
    } else if ("age" %in% original_columns) {
      data <- data %>% left_join(load_sex_age_data()$ages)
      input_set <- c(input_set, "age")
    }
  } else {
    if ("last_name" %in% original_columns) {
      data <- data %>% left_join(bper_data$last_names)
      input_set <- c(input_set, "last")
    }
    if ("first_name" %in% original_columns) {
      data <- data %>% left_join(bper_data$first_names)
      input_set <- c(input_set, "first")
    }
    if ("party" %in% original_columns) {
      data <- data %>% left_join(bper_data$parties)
      input_set <- c(input_set, "party")
    }
    if ("multi_unit" %in% original_columns) {
      data <- data %>% left_join(bper_data$multi_units)
      input_set <- c(input_set, "multi-unit")
    }
    if ("sex" %in% original_columns
        & "age" %in% original_columns) {
      data <- data %>% left_join(bper_data$sex_ages$sex_ages)
      input_set <- c(input_set, "sex-age")
    } else if ("sex" %in% original_columns) {
      data <- data %>% left_join(bper_data$sex_ages$sexes)
      input_set <- c(input_set, "sex")
    } else if ("age" %in% original_columns) {
      data <- data %>% left_join(bper_data$sex_ages$ages)
      input_set <- c(input_set, "age")
    }
  }

  data <- bper_naive_bayes(data, priors_set = input_set)

  data <- data %>%
    select(all_of(original_columns), contains("pred"))

  return(data)
}




Sys.setenv(CENSUS_KEY = "5e4c2b8438222753a7f4753fa78855eca73b9950")
readRenviron("~/.Renviron")

load_surnames_data <- function(year) {

  psuedo_count <- 1

  last_names <- censusapi::getCensus(
    name = "surname",
    vintage = year,
    vars = c(
      "COUNT",
      "PCTWHITE",
      "PCTBLACK",
      "PCTAIAN",
      "PCTAPI",
      "PCTHISPANIC",
      "PCT2PRACE"
    ),
    NAME = "*"
  ) %>%
    rename_with( ~ tolower(.), everything()) %>%
    mutate(
      count = as.numeric(count),
      across(contains("pct"),
             ~ replace_na(as.numeric(.), 0) / 100),
      across(contains("pct"),
             ~ (. * count) + psuedo_count,
             .names = "count_{.col}"),
      across(starts_with("count_"), ~ . / count,
             .names = "pr_{.col}|s"),
      across(starts_with("count_"),
             ~ . / sum(.),
             .names = "pr_s|{.col}")
    ) %>%
    rename_with( ~ str_remove_all(., "count_pct"), contains("pr_")) %>%
    rename_with( ~ str_replace_all(., "2prace", "other"), everything()) %>%
    select(last_name = name, contains("pr_"))

  return(last_names)
}


get_sex_age_data <- function(census_group, group_name, year) {

  labels <- censusapi::listCensusMetadata(
    name = "dec/sf1",
    vintage = year,
    type = "variables",
    group = census_group) %>%
    filter(predicateType == "int") %>%
    arrange(name)

  sex_age_df <- censusapi::getCensus(
    name = "dec/sf1",
    vintage = year,
    region = "us",
    vars = paste0("group(", census_group, ")")
  ) %>%
    select(labels$name) %>%
    `colnames<-`(labels$label) %>%
    pivot_longer(cols = everything(),
                 values_to = group_name,
                 names_repair = "minimal")

  return(sex_age_df)
}

load_sex_age_data <- function(year, vars) {

  if (year == 2010) {
    sex_age_totals <- get_sex_age_data("PCT12", "total", 2010) %>%
      left_join(get_sex_age_data("PCT12H", "hispanic", 2010)) %>%
      left_join(get_sex_age_data("PCT12I", "white", 2010)) %>%
      left_join(get_sex_age_data("PCT12J", "black", 2010)) %>%
      left_join(get_sex_age_data("PCT12K", "aian", 2010)) %>%
      left_join(get_sex_age_data("PCT12L", "api1", 2010)) %>%
      left_join(get_sex_age_data("PCT12M", "api2", 2010)) %>%
      left_join(get_sex_age_data("PCT12N", "other1", 2010)) %>%
      left_join(get_sex_age_data("PCT12O", "other2", 2010)) %>%
      mutate(api = api1 + api2,
             other = other1 + other2) %>%
      select(-c(api1, api2, other1, other2))
  }

  if (vars == "both") {

    sex_age <- sex_age_totals %>%
      filter(!name %in% c("Total", "Total!!Female", "Total!!Male")) %>%
      mutate(
        sex = if_else(grepl("Female", name), 1, 0),
        age = if_else(grepl("Under", name), 0,
                      as.numeric(str_extract(name, "[:digit:]+"))),
        across(c(hispanic, white, black, aian, api, other),
               ~ . / total,
               .names = "pr_{.col}|sex_age"),
        across(c(hispanic, white, black, aian, api, other),
               ~ . / sum(.),
               .names = "pr_sex_age|{.col}")
      ) %>%
      select(sex, age, contains("pr_"))

    return(sex_age)

  } else if (vars == "sex") {

    sex <- sex_age_totals %>%
      filter(name %in% c("Total!!Male", "Total!!Female")) %>%
      mutate(
        sex = if_else(grepl("Female", name), 1, 0),
        across(c(hispanic, white, black, aian, api, other),
               ~ . / total,
               .names = "pr_{.col}|sex"),
        across(c(hispanic, white, black, aian, api, other),
               ~ . / sum(.),
               .names = "pr_sex|{.col}")
      ) %>%
      select(sex, contains("pr_"))

    return(sex)

  } else if (vars == "age") {

    age <- sex_age_totals %>%
      filter(!name %in% c("Total", "Total!!Female", "Total!!Male")) %>%
      mutate(age = if_else(grepl("Under", name), 0,
                           as.numeric(str_extract(name, "[:digit:]+")))) %>%
      group_by(age) %>%
      summarise(across(-name, sum)) %>%
      ungroup() %>%
      mutate(
        across(c(hispanic, white, black, aian, api, other),
               ~ . / total,
               .names = "pr_{.col}|age"),
        across(c(hispanic, white, black, aian, api, other),
               ~ . / sum(.),
               .names = "pr_age|{.col}")
      ) %>%
      select(age, contains("pr_"))

    return(age)

  }
}

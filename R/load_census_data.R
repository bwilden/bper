
Sys.setenv(CENSUS_KEY = "5e4c2b8438222753a7f4753fa78855eca73b9950")
readRenviron("~/.Renviron")


# Last Names --------------------------------------------------------------

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
             .names = "pr_{.col}|last"),
      across(starts_with("count_"),
             ~ . / sum(.),
             .names = "pr_last|{.col}")
    ) %>%
    rename_with( ~ str_remove_all(., "count_pct"), contains("pr_")) %>%
    rename_with( ~ str_replace_all(., "2prace", "other"), everything()) %>%
    select(last_name = name, contains("pr_"))

  return(last_names)
}


# Multi-Unit Occupancy ----------------------------------------------------

load_multi_unit_data <- function(year, census_groups) {

  multi_units <- tibble()
  for (group in census_groups) {
    group_multi_units <- censusapi::getCensus(
      name = "acs/acs5",
      vintage = year,
      region = "us",
      vars = paste0("group(", group[1], ")")
    ) %>%
      select(ends_with("E"), -NAME) %>%
      rename_with(~ str_remove(., group[1])) %>%
      mutate(group := group[2])

    multi_units <- rbind(multi_units, group_multi_units)
  }

  ethnorace_columns <- unlist(census_groups)[c(F, T)]

  multi_units <- multi_units %>%
    pivot_longer(cols = !group) %>%
    pivot_wider(names_from = group) %>%
    filter(name != "_001E") %>%
    mutate(multi_unit = if_else(name %in% c("_002E", "_003E", "_010E", "_011E"), 0, 1)) %>%
    group_by(multi_unit) %>%
    summarise(across(-name, sum)) %>%
    ungroup() %>%
    mutate(
      across(ethnorace_columns,
             ~ . / rowSums(across(ethnorace_columns)),
             .names = "pr_{.col}|multi_unit"),
      across(ethnorace_columns,
             ~ . / sum(.),
             .names = "pr_multi_unit|{.col}")
    ) %>%
    select(multi_unit, contains("pr_"))

  return(multi_units)
}


# Sex/Age -----------------------------------------------------------------

load_sex_age_data <- function(year, census_groups, vars) {

  sex_age_totals <- tibble()
  for (group in census_groups) {

    sex_age_totals_group <- censusapi::getCensus(
      name = "dec/sf1",
      vintage = year,
      region = "us",
      vars = paste0("group(", group[1], ")")
    ) %>%
      select(contains("PCT")) %>%
      rename_with(~ str_sub(., start = 8)) %>%
      mutate(group := group[2])

    sex_age_totals <- rbind(sex_age_totals, sex_age_totals_group)
  }

  labels <- censusapi::listCensusMetadata(
    name = "dec/sf1",
    vintage = year,
    type = "variables",
    group = census_groups[[1]][1]) %>%
    mutate(name = str_sub(name, start = 8)) %>%
    select(name, label)

  ethnorace_columns <- unlist(census_groups)[c(F, T)]

  sex_age_totals <- sex_age_totals %>%
    pivot_longer(cols = !group) %>%
    pivot_wider(names_from = group) %>%
    left_join(labels, by = "name")

  sex_ages <- sex_age_totals %>%
    filter(!label %in% c("Total", "Total!!Female", "Total!!Male")) %>%
    mutate(
      sex = if_else(grepl("Female", label), 1, 0),
      age = if_else(grepl("Under", label), 0,
                    as.numeric(str_extract(label, "[:digit:]+"))),
      across(ethnorace_columns,
             ~ . / rowSums(across(ethnorace_columns)),
             .names = "pr_{.col}|sex_age"),
      across(ethnorace_columns,
             ~ . / sum(.),
             .names = "pr_sex_age|{.col}")
    ) %>%
    select(sex, age, contains("pr_"))

  sexes <- sex_age_totals %>%
    filter(label %in% c("Total!!Male", "Total!!Female")) %>%
    mutate(
      sex = if_else(grepl("Female", label), 1, 0),
      across(ethnorace_columns,
             ~ . / rowSums(across(ethnorace_columns)),
             .names = "pr_{.col}|sex"),
      across(ethnorace_columns,
             ~ . / sum(.),
             .names = "pr_sex|{.col}")
    ) %>%
    select(sex, contains("pr_"))

  ages <- sex_age_totals %>%
    filter(!label %in% c("Total", "Total!!Female", "Total!!Male")) %>%
    mutate(age = if_else(grepl("Under", label), 0,
                         as.numeric(str_extract(label, "[:digit:]+")))) %>%
    group_by(age) %>%
    summarise(across(-c(name, label), sum)) %>%
    ungroup() %>%
    mutate(
      across(ethnorace_columns,
             ~ . / rowSums(across(ethnorace_columns)),
             .names = "pr_{.col}|age"),
      across(ethnorace_columns,
             ~ . / sum(.),
             .names = "pr_age|{.col}")
    ) %>%
    select(age, contains("pr_"))

  if (vars == "both") {
    return(sex_ages)
  } else if (vars == "sex") {
    return(sexes)
  } else if (vars == "age") {
    return(ages)
  }
}



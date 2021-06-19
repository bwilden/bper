
Sys.setenv(CENSUS_KEY = "5e4c2b8438222753a7f4753fa78855eca73b9950")
readRenviron("~/.Renviron")

# Last Names --------------------------------------------------------------

load_surnames_data <- function(year = 2010, psuedocount = 1) {

  suppressWarnings(
    last_name <- censusapi::getCensus(
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
               ~ (. * count) + psuedocount,
               .names = "count_{.col}"),
        across(starts_with("count_"),
               ~ . / count,
               .names = "pr_{.col}|last"),
        across(starts_with("count_"),
               ~ . / sum(.),
               .names = "pr_last|{.col}")
      ) %>%
      rename_with( ~ str_remove_all(., "count_pct"), contains("pr_")) %>%
      rename_with( ~ str_replace_all(., "2prace", "other"), everything()) %>%
      select(last_name = name, contains("pr_"))
  )

  return(last_name)
}


# First Names -------------------------------------------------------------

load_first_names_data <- function(psuedocount = 1) {

  first_name <- first_name %>%
    mutate(
      across(contains("pct"),
             ~ as.numeric(.) / 100),
      across(contains("pct"),
             ~ (. * obs) + psuedocount,
             .names = "obs_{.col}"),
      across(starts_with("obs_"),
             ~ . / obs,
             .names = "pr_{.col}|first"),
      across(starts_with("obs_"),
             ~ . / sum(.),
             .names = "pr_first|{.col}")
    ) %>%
    rename_with( ~ str_remove_all(., "obs_pct"), contains("pr_")) %>%
    rename_with( ~ str_replace_all(., "2prace", "other"), everything()) %>%
    select(first_name = firstname, contains("pr_"))

  return(first_name)
}


# Party ID ----------------------------------------------------------------

load_parties_data <- function(year = 2020) {

  if (year <= 2005) {
    year <- 2000
  } else if (year <= 2015) {
    year <- 2010
  } else {
    year <- 2020
  }

  party <- anes %>%
    filter(year_group == year) %>%
    mutate(
      across(ethnorace_set,
             ~ . / rowSums(across(ethnorace_set)),
             .names = "pr_{.col}|party"),
      across(ethnorace_set,
             ~ . / sum(.),
             .names = "pr_party|{.col}")
    ) %>%
    select(party, contains("pr_"))

  return(party)
}


# Multi-Unit Occupancy ----------------------------------------------------

load_multi_unit_data <- function(year = 2019) {

  census_groups = list(
    c("B25032A", "white"),
    c("B25032B", "black"),
    c("B25032C", "aian"),
    c("B25032D", "api"),
    c("B25032E", "api"),
    c("B25032F", "other"),
    c("B25032G", "other"),
    c("B25032I", "hispanic")
  )

  multi_unit <- tibble()
  for (group in census_groups) {
    group_multi_unit <- censusapi::getCensus(
      name = "acs/acs5",
      vintage = year,
      region = "us",
      vars = paste0("group(", group[1], ")")
    ) %>%
      select(ends_with("E"), -NAME) %>%
      rename_with(~ str_remove(., group[1])) %>%
      mutate(group := group[2])

    multi_unit <- rbind(multi_unit, group_multi_unit)
  }

  multi_unit <- multi_unit %>%
    group_by(group) %>%
    summarise(across(everything(), sum)) %>%
    ungroup() %>%
    pivot_longer(cols = !group) %>%
    pivot_wider(names_from = group) %>%
    filter(name != "_001E") %>%
    mutate(multi_unit = if_else(name %in% c("_002E", "_003E", "_010E", "_011E"), 0, 1)) %>%
    group_by(multi_unit) %>%
    summarise(across(-name, sum)) %>%
    ungroup() %>%
    mutate(
      across(ethnorace_set,
             ~ . / rowSums(across(ethnorace_set)),
             .names = "pr_{.col}|multi-unit"),
      across(ethnorace_set,
             ~ . / sum(.),
             .names = "pr_multi-unit|{.col}")
    ) %>%
    select(multi_unit, contains("pr_"))

  return(multi_unit)
}


# Sex/Age -----------------------------------------------------------------

load_sex_age_data <- function(year = 2010) {

  census_groups = list(
    c("PCT12I", "white"),
    c("PCT12J", "black"),
    c("PCT12K", "aian"),
    c("PCT12L", "api"),
    c("PCT12M", "api"),
    c("PCT12N", "other"),
    c("PCT12O", "other"),
    c("PCT12H", "hispanic")
  )

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

  sex_age_totals <- sex_age_totals %>%
    group_by(group) %>%
    summarise(across(everything(), sum)) %>%
    ungroup() %>%
    pivot_longer(cols = !group) %>%
    pivot_wider(names_from = group) %>%
    left_join(labels, by = "name")

  sex_age <- sex_age_totals %>%
    filter(!label %in% c("Total", "Total!!Female", "Total!!Male")) %>%
    mutate(
      sex = if_else(grepl("Female", label), 1, 0),
      age = if_else(grepl("Under", label), 0,
                    as.numeric(str_extract(label, "[:digit:]+"))),
      across(ethnorace_set,
             ~ . / rowSums(across(ethnorace_set)),
             .names = "pr_{.col}|sex-age"),
      across(ethnorace_set,
             ~ . / sum(.),
             .names = "pr_sex-age|{.col}")
    ) %>%
    select(sex, age, contains("pr_"))

  sex <- sex_age_totals %>%
    filter(label %in% c("Total!!Male", "Total!!Female")) %>%
    mutate(
      sex = if_else(grepl("Female", label), 1, 0),
      across(ethnorace_set,
             ~ . / rowSums(across(ethnorace_set)),
             .names = "pr_{.col}|sex"),
      across(ethnorace_set,
             ~ . / sum(.),
             .names = "pr_sex|{.col}")
    ) %>%
    select(sex, contains("pr_"))

  age <- sex_age_totals %>%
    filter(!label %in% c("Total", "Total!!Female", "Total!!Male")) %>%
    mutate(age = if_else(grepl("Under", label), 0,
                         as.numeric(str_extract(label, "[:digit:]+")))) %>%
    group_by(age) %>%
    summarise(across(-c(name, label), sum)) %>%
    ungroup() %>%
    mutate(
      across(ethnorace_set,
             ~ . / rowSums(across(ethnorace_set)),
             .names = "pr_{.col}|age"),
      across(ethnorace_set,
             ~ . / sum(.),
             .names = "pr_age|{.col}")
    ) %>%
    select(age, contains("pr_"))

  sex_age_data <- list(
    "sex_age" = sex_age,
    "sex" = sex,
    "age" = age
  )
  return(sex_age_data)
}


# Geo ---------------------------------------------------------------------

load_acs5_data <- function(geo_level, states, year = 2019, psuedocount = 1) {
  if (geo_level == "zip") {
    geo_level <- "zip code tabulation area"
  }

  if (states == "all") {
    state_codes <- state_codes %>%
      filter(state %notin% c("AS", "GU", "MP", "PR", "UM", "VI")) %>%
      pull(GEO_ID)
  } else {
    state_codes <- state_codes %>%
      filter(state %in% states) %>%
      pull(GEO_ID)
  }
  geo <- tibble()

  for (state in state_codes) {
    state_data <- censusapi::getCensus(
      name = "acs/acs5",
      vintage = year,
      vars = "group(B03002)",
      region = geo_level,
      regionin = paste("state", state, sep = ":")
    ) %>%
      mutate(
        white = B03002_003E,
        black = B03002_004E,
        aian = B03002_005E,
        api = B03002_006E + B03002_007E,
        other = B03002_008E + B03002_009E + B03002_010E + B03002_011E,
        hispanic = B03002_012E
      ) %>%
      mutate(
        across(ethnorace_set,
               ~ (. + psuedocount) / (rowSums(
                 across(ethnorace_set)
               ) + psuedocount * 6),
               .names = "pr_{.col}|geo"),
        across(ethnorace_set,
               ~ (. + psuedocount) / sum(. + psuedocount * n()),
               .names = "pr_geo|{.col}")
      ) %>%
      select(GEO_ID, contains("pr_"))
    geo <- rbind(geo, state_data)
  }

  # Convert GEO_ID to appropriate columns
  if (geo_level == "county") {
    geo <- geo %>%
      mutate(state = str_sub(GEO_ID, start = -5, end = -4),
             county = str_sub(GEO_ID, start = -3)) %>%
      select(state, county, everything(), -GEO_ID)
  } else if (geo_level == "zip code tabulation area") {
    geo <- geo %>%
      mutate(zip = str_sub(GEO_ID, start = -5)) %>%
      select(zip, everything(), -GEO_ID)
  } else if (geo_level == "place") {
    geo <- geo %>%
      mutate(state = str_sub(GEO_ID, start = -7, end = -6),
             place = str_sub(GEO_ID, start = -5)) %>%
      select(state, place, everything(), -GEO_ID)
  } else if (geo_level == "tract") {
    geo <- geo %>%
      mutate(state = str_sub(GEO_ID, start = -11, end = -10),
             county = str_sub(GEO_ID, start = -9, end = -7),
             tract = str_sub(GEO_ID, start = -6)) %>%
      select(state, county, tract, everything(), -GEO_ID)
  } else if (geo_level == "congressional district") {
    geo <- geo %>%
      mutate(state = str_sub(GEO_ID, start = -4, end = -3),
             district = str_sub(GEO_ID, start = -2)) %>%
      select(state, district, everything(), -GEO_ID)
  }
  return(geo)
}


# censusapi::listCensusMetadata(
#   name = "acs/acs5",
#   vintage = 2019,
#   type = "geographies",
#   group = "B03002") %>%
#   View()

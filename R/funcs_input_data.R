
Sys.setenv(CENSUS_KEY = "5e4c2b8438222753a7f4753fa78855eca73b9950")
readRenviron("~/.Renviron")

ethnorace_set <- c("aian", "api", "black", "hispanic", "other", "white")

# Last Names --------------------------------------------------------------

load_surnames_data <- function(year = 2010, psuedocount = 1) {

  suppressWarnings(
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

  return(last_names)
}


# First Names -------------------------------------------------------------

load_first_names_data <- function(psuedocount = 1) {

  first_names <- first_names %>%
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

  return(first_names)
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

  parties <- anes %>%
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

  return(parties)
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

  multi_units <- multi_units %>%
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
             .names = "pr_{.col}|multi_unit"),
      across(ethnorace_set,
             ~ . / sum(.),
             .names = "pr_multi_unit|{.col}")
    ) %>%
    select(multi_unit, contains("pr_"))

  return(multi_units)
}


# Sex/Age -----------------------------------------------------------------

load_sex_age_data <- function(year = 2010, vars = "both") {

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

  sex_ages <- sex_age_totals %>%
    filter(!label %in% c("Total", "Total!!Female", "Total!!Male")) %>%
    mutate(
      sex = if_else(grepl("Female", label), 1, 0),
      age = if_else(grepl("Under", label), 0,
                    as.numeric(str_extract(label, "[:digit:]+"))),
      across(ethnorace_set,
             ~ . / rowSums(across(ethnorace_set)),
             .names = "pr_{.col}|sex_age"),
      across(ethnorace_set,
             ~ . / sum(.),
             .names = "pr_sex_age|{.col}")
    ) %>%
    select(sex, age, contains("pr_"))

  sexes <- sex_age_totals %>%
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

  ages <- sex_age_totals %>%
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

  if (vars == "both") {
    return(sex_ages)
  } else if (vars == "sex") {
    return(sexes)
  } else if (vars == "age") {
    return(ages)
  }
}


# State -------------------------------------------------------------------

load_geo_data <- function(geo, year = 2019, psuedocount = 1) {
  geos <- censusapi::getCensus(
    name = "acs/acs5",
    vintage = year,
    vars = "group(B03002)",
    region = geo,
  ) %>%
    mutate(white = B03002_003E,
           black = B03002_004E,
           aian = B03002_005E,
           api = B03002_006E + B03002_007E,
           other = B03002_008E + B03002_009E + B03002_010E + B03002_011E,
           hispanic = B03002_012E) %>%
    mutate(
      across(ethnorace_set,
             ~ . / (rowSums(across(ethnorace_set)) + psuedocount),
             .names = "pr_{.col}|geo"),
      across(ethnorace_set,
             ~ . / sum(.),
             .names = "pr_geo|{.col}")
    ) %>%
    select(GEO_ID, contains("pr_"))

  if (geo == "state") {
    geos <- geos %>%
      mutate(GEO_ID = str_sub(GEO_ID, start = -2))
  }

  return(geos)
}

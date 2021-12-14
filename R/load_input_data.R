
# Last Names --------------------------------------------------------------

load_surnames_data <- function(year, psuedocount = 1) {
  data_years <- c(2000, 2010)
  closest_year <- data_years[which.min(abs(data_years - year))]

  suppressWarnings(
    last_names <- censusapi::getCensus(
      name = "surname",
      vintage = closest_year,
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
      rename(PCTAAPI = PCTAPI) %>%
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

load_parties_data <- function(year) {
  data_years <- c(2000, 2010, 2020)
  closest_year <- data_years[which.min(abs(data_years - year))]

  parties <- anes %>%
    filter(year_group == closest_year) %>%
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

load_multi_unit_data <- function(year) {
  data_years <- c(2000, 2009:2019)
  closest_year <- data_years[which.min(abs(data_years - year))]

  if (closest_year >= 2009) {
    census_file <- "acs/acs5"
    census_groups <- list(
      c("B25032A", "white"),
      c("B25032B", "black"),
      c("B25032C", "aian"),
      c("B25032D", "aapi"),
      c("B25032E", "aapi"),
      c("B25032F", "other"),
      c("B25032G", "other"),
      c("B25032I", "hispanic")
    )
  } else if (closest_year == 2000) {
    census_file <- "dec/sf3"
    census_groups <- list(
      c("HCT030E", "aapi"),
      c("HCT030F", "other"),
      c("HCT030G", "other"),
      c("HCT030B", "black"),
      c("HCT030C", "aian"),
      c("HCT030D", "aapi"),
      c("HCT030I", "white"),
      c("HCT030H", "hispanic")
    )
  }

  multi_units <- tibble()
  for (group in census_groups) {
    group_multi_unit <- censusapi::getCensus(
      name = census_file,
      vintage = closest_year,
      region = "us",
      vars = paste0("group(", group[1], ")")
    ) %>%
      select(-ends_with(c("A", "M"))) %>%
      rename_with( ~ str_extract(str_remove(., group[1]),
                                 "\\d+"), contains(group[1])) %>%
      mutate(single_unit = `002` + `003` + `010` + `011`,
             multi_unit = `001` - single_unit,
             group := group[2]) %>%
      select(group, multi_unit, single_unit)

    multi_units <- rbind(multi_units, group_multi_unit)
  }

  multi_units <- multi_units %>%
    group_by(group) %>%
    summarise(across(everything(), sum)) %>%
    ungroup() %>%
    pivot_longer(cols = !group) %>%
    pivot_wider(names_from = group) %>%
    mutate(multi_unit = if_else(name == "multi_unit", 1, 0),
      across(all_of(ethnorace_set),
             ~ . / rowSums(across(ethnorace_set)),
             .names = "pr_{.col}|multi-unit"),
      across(all_of(ethnorace_set),
             ~ . / sum(.),
             .names = "pr_multi-unit|{.col}")
    ) %>%
    select(multi_unit, contains("pr_"))

  return(multi_units)
}


# Sex/Age -----------------------------------------------------------------

load_sex_age_data <- function(year) {
  data_years <- c(2000, 2010)
  closest_year <- data_years[which.min(abs(data_years - year))]

  if (closest_year == 2010) {
    census_groups <- list(
      c("PCT12I", "white"),
      c("PCT12J", "black"),
      c("PCT12K", "aian"),
      c("PCT12L", "aapi"),
      c("PCT12M", "aapi"),
      c("PCT12N", "other"),
      c("PCT12O", "other"),
      c("PCT12H", "hispanic")
    )
  } else if (closest_year == 2000) {
    census_groups <- list(
      c("PCT012I", "white"),
      c("PCT012J", "black"),
      c("PCT012K", "aian"),
      c("PCT012L", "aapi"),
      c("PCT012M", "aapi"),
      c("PCT012N", "other"),
      c("PCT012O", "other"),
      c("PCT012H", "hispanic")
    )
  }

  sex_age_totals <- tibble()
  for (group in census_groups) {
    sex_age_totals_group <- censusapi::getCensus(
      name = "dec/sf1",
      vintage = closest_year,
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
    vintage = closest_year,
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
             .names = "pr_{.col}|sex-age"),
      across(ethnorace_set,
             ~ . / sum(.),
             .names = "pr_sex-age|{.col}")
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

  sex_age_data <- list(
    "sex_ages" = sex_ages,
    "sexes" = sexes,
    "ages" = ages
  )
  return(sex_age_data)
}


# Geo ---------------------------------------------------------------------

load_geo_data <- function(geo_level, states = NULL, year, psuedocount = 1) {
  # Set closest year from years data is available
  if (geo_level == "block") {
    data_years <- c(2000, 2010)
  } else if (geo_level == "zip") {
    data_years <- c(2000, 2011:2019)
  } else {
    data_years <- c(2000, 2009:2019)
  }
  closest_year <- data_years[which.min(abs(data_years - year))]

  # Set census file and variable names
  if (closest_year >= 2009) {
    census_file <- "acs/acs5"
    census_vars <- "group(B03002)"
  } else if (closest_year < 2009) {
    census_file <- "dec/sf1"
    census_vars <- "group(P008)"
  }

  # Set census geographies
  if (geo_level == "zip") {
    census_geo <- "zip code tabulation area"
  } else if (geo_level == "district") {
    census_geo <- "congressional district"
  } else {
    census_geo <- geo_level
  }

  # Set census state codes
  if (geo_level %in% c("state", "zip")) {
    state_codes <- ""
  } else if (states[1] == "all") {
    state_codes <- state_codes %>%
      filter(state %notin% c("AS", "GU", "MP", "PR", "UM", "VI")) %>%
      pull(state_code)
  } else {
    state_codes <- state_codes %>%
      filter(state %in% states) %>%
      pull(state_code)
  }

  # Placeholder tibble for combined state geo data
  geo <- tibble()
  # Loop through each state
  for (state in state_codes) {
    # For block geo_level grab data from github repo
    # https://github.com/bwilden/bper_data
    if (geo_level == "block") {
      state_data <- readr::read_csv(
        paste0("https://github.com/bwilden/bper_data/blob/master/data/",
               closest_year,
               "/blocks_",
               closest_year,
               "_",
               state,
               ".csv?raw=true"),
        col_types = readr::cols()
      ) %>%
        unite(GEO_ID, "state_code", "county", "tract", "block")
    # Non-block geo_level access through census api
    } else {
      if (geo_level %in% c("state", "zip")) {
        census_regions <- NULL
      } else {
        census_regions <- paste("state", state, sep = ":")
      }
      state_data <- censusapi::getCensus(
        name = census_file,
        vintage = closest_year,
        region = census_geo,
        regionin = census_regions,
        vars = census_vars
      )
      # Get ethnorace count variables by geo unit
      if (census_vars == "group(B03002)") {
        state_data <- state_data %>%
          mutate(
            white = B03002_003E,
            black = B03002_004E,
            aian = B03002_005E,
            aapi = B03002_006E + B03002_007E,
            other = B03002_008E + B03002_009E + B03002_010E + B03002_011E,
            hispanic = B03002_012E
          )
      } else if (census_vars == "group(P008)") {
        state_data <- state_data %>%
          mutate(
            white = P008003,
            black = P008004,
            aian = P008005,
            aapi = P008006 + P008007,
            other = P008009 + P008008,
            hispanic = P008010
          )
      }
    }
    # Perform conditional probability calculations
    state_data <- state_data %>%
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
  if (geo_level == "state") {
    geo <- geo %>%
      mutate(state_code = str_sub(GEO_ID, start = -2)) %>%
      select(state_code, everything(), -GEO_ID)
  } else if (geo_level == "county") {
    geo <- geo %>%
      mutate(state_code = str_sub(GEO_ID, start = -5, end = -4),
             county = str_sub(GEO_ID, start = -3)) %>%
      select(state_code, county, everything(), -GEO_ID)
  } else if (geo_level == "zip") {
    geo <- geo %>%
      mutate(zip = str_sub(GEO_ID, start = -5)) %>%
      select(zip, everything(), -GEO_ID)
  } else if (geo_level == "place") {
    geo <- geo %>%
      mutate(state_code = str_sub(GEO_ID, start = -7, end = -6),
             place = str_sub(GEO_ID, start = -5)) %>%
      select(state_code, place, everything(), -GEO_ID)
  } else if (geo_level == "tract") {
    geo <- geo %>%
      mutate(state_code = str_sub(GEO_ID, start = -11, end = -10),
             county = str_sub(GEO_ID, start = -9, end = -7),
             tract = str_sub(GEO_ID, start = -6)) %>%
      select(state_code, county, tract, everything(), -GEO_ID)
  } else if (geo_level == "district") {
    geo <- geo %>%
      mutate(state_code = str_sub(GEO_ID, start = -4, end = -3),
             district = str_sub(GEO_ID, start = -2)) %>%
      select(state_code, district, everything(), -GEO_ID)
  } else if (geo_level == "block") {
    geo <- geo %>%
      separate(GEO_ID, into = c("state_code", "county", "tract", "block"))
  }

  # Tidy up column names
  geo <- geo %>%
    rename_with(~ str_replace_all(., "geo", geo_level), everything())

  return(geo)
}

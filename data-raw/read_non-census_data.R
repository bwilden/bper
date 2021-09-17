
# First names ethnorace data
# Tzioumis (2018); https://www.nature.com/articles/sdata201825
# first_names <- readxl::read_xlsx(here::here("data-raw", "firstnames.xlsx"),
#                                  sheet = "Data")
#
# # American National Election Survey for political party ethnorace data
# # https://electionstudies.org/data-center/anes-time-series-cumulative-data-file/
# anes <- haven::read_dta(here::here("data-raw", "anes_timeseries_cdf.dta")) %>%
#   filter(VCF0004 >= 1996) %>%
#   select(year = VCF0004, ethnorace = VCF0105a, party = VCF0303) %>%
#   mutate(party = if_else(is.na(party), 2, as.numeric(party)),
#          party = case_when(party == 1 ~ "DEM",
#                           party == 2 ~ "UNA",
#                           party == 3 ~ "REP"),
#          ethnorace = case_when(ethnorace == 1 ~ "white",
#                                ethnorace == 2 ~ "black",
#                                ethnorace == 3 ~ "api",
#                                ethnorace == 4 ~ "aian",
#                                ethnorace == 5 ~ "hispanic",
#                                ethnorace == 6 ~ "other",
#                                TRUE ~ NA_character_),
#          year_group = case_when(year <= 2005 ~ "2000",
#                                 year <= 2015 ~ "2010",
#                                 TRUE ~ "2020")) %>%
#   group_by(year_group, party) %>%
#   summarise(white = sum(ethnorace == "white", na.rm = T),
#             black = sum(ethnorace == "black", na.rm = T),
#             api = sum(ethnorace == "api", na.rm = T),
#             aian = sum(ethnorace == "aian", na.rm = T),
#             hispanic = sum(ethnorace == "hispanic", na.rm = T),
#             other = sum(ethnorace == "other", na.rm = T)) %>%
#   ungroup()
#
# # State code concordance file
# state_codes <- readr::read_csv(here::here("data-raw", "state_code_conc.csv")) %>%
#   mutate(state_code = str_pad(STATE, width = 2, pad = "0")) %>%
#   select(state_code, state = STUSAB)

# Possible ethnorace outputs
ethnorace_set <- c("aian", "api", "black", "hispanic", "other", "white")

# Possible input data columns
bper_vars <- c("last_names", "first_names", "sex_ages", "ages", "sexes", "parties", "multi_units",
               "counties", "states", "places", "zips", "tracts", "blocks", "districts")

# Possible geographic levels
bper_geos <- c("state", "county", "zip", "place", "tract", "block", "district")


# usethis::use_data(first_names, anes, state_codes,
#                   bper_vars, ethnorace_set, bper_geos,
#                   internal = TRUE,
#                   overwrite = TRUE,
#                   compress = "gzip",
#                   version = 3)

usethis::use_data(ethnorace_set, bper_vars, bper_geos,
                  internal = T,
                  overwrite = T)

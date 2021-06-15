
first_names <- readxl::read_xlsx(here::here("data-raw", "firstnames.xlsx"),
                                 sheet = "Data")

anes <- haven::read_dta(here::here("data-raw", "anes_timeseries_cdf.dta")) %>%
  filter(VCF0004 >= 1996) %>%
  select(year = VCF0004, ethnorace = VCF0105a, party = VCF0303) %>%
  mutate(party = if_else(is.na(party), 2, as.numeric(party)),
         party = case_when(party == 1 ~ "DEM",
                          party == 2 ~ "UNA",
                          party == 3 ~ "REP"),
         ethnorace = case_when(ethnorace == 1 ~ "white",
                               ethnorace == 2 ~ "black",
                               ethnorace == 3 ~ "api",
                               ethnorace == 4 ~ "aian",
                               ethnorace == 5 ~ "hispanic",
                               ethnorace == 6 ~ "other",
                               TRUE ~ NA_character_),
         year_group = case_when(year <= 2005 ~ "2000",
                                year <= 2015 ~ "2010",
                                TRUE ~ "2020")) %>%
  group_by(year_group, party) %>%
  summarise(white = sum(ethnorace == "white", na.rm = T),
            black = sum(ethnorace == "black", na.rm = T),
            api = sum(ethnorace == "api", na.rm = T),
            aian = sum(ethnorace == "aian", na.rm = T),
            hispanic = sum(ethnorace == "hispanic", na.rm = T),
            other = sum(ethnorace == "other", na.rm = T)) %>%
  ungroup()

usethis::use_data(first_names, anes, overwrite = TRUE, internal = TRUE)
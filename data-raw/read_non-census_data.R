
first_names <- readxl::read_xlsx(here::here("data-raw", "firstnames.xlsx"),
                                 sheet = "Data")

anes <- haven::read_dta(here::here("data-raw", "anes_timeseries_cdf.dta")) %>%
  filter(VCF0004 >= 1996) %>%
  select(year = VCF0004, ethnorace = VCF0105a, pid3 = VCF0303) %>%
  mutate(pid3 = if_else(is.na(pid3), 2, as.numeric(pid3)),
         pid3 = case_when(pid3 == 1 ~ "DEM",
                          pid3 == 2 ~ "UNA",
                          pid3 == 3 ~ "REP"),
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
  group_by(year_group, pid3) %>%
  summarise(white = sum(ethnorace == "white", na.rm = T),
            black = sum(ethnorace == "black", na.rm = T),
            api = sum(ethnorace == "api", na.rm = T),
            aian = sum(ethnorace == "aian", na.rm = T),
            hispanic = sum(ethnorace == "hispanic", na.rm = T),
            other = sum(ethnorace == "other", na.rm = T))

usethis::use_data(first_names, anes, overwrite = TRUE, internal = TRUE)

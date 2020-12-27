

# First Names -------------------------------------------------------------

firstnames <- readxl::read_xlsx("data-raw/firstnames.xlsx", sheet = "Data") %>%
  dplyr::mutate_at(dplyr::vars(dplyr::contains("pct")), function(x) x / 100) %>%
  dplyr::mutate(
    obs = obs + 6,
    white_f = pctwhite * obs + 1,
    black_f = pctblack * obs + 1,
    hispanic_f = pcthispanic * obs + 1,
    api_f = pctapi * obs + 1,
    aian_f = pctaian * obs + 1,
    other_f = pct2prace * obs + 1,
    pr_other_f = other_f / obs,
    pr_white_f = white_f / obs,
    pr_black_f = black_f / obs,
    pr_hispanic_f = hispanic_f / obs,
    pr_api_f = api_f / obs,
    pr_aian_f = aian_f / obs,
    pr_f_white = white_f / sum(white_f),
    pr_f_black = black_f / sum(black_f),
    pr_f_hispanic = hispanic_f / sum(hispanic_f),
    pr_f_api = api_f / sum(api_f),
    pr_f_aian = aian_f / sum(aian_f),
    pr_f_other = other_f / sum(other_f)
  ) %>%
  dplyr::select(first_name = firstname, dplyr::contains("pr_"))


# Surnames ----------------------------------------------------------------

surnames_2010 <- readr::read_csv("data-raw/Names_2010Census.csv",
                                 col_types = readr::cols(name = readr::col_character(),
                                                         .default = readr::col_double()))

surnames <- readr::read_csv("data-raw/Names_2000Census.csv",
                             col_types = readr::cols(name = readr::col_character(),
                                                     .default = readr::col_double())) %>%
  dplyr::filter(!(name %in% surnames_2010$name)) %>%
  rbind(surnames_2010) %>%
  dplyr::mutate_at(dplyr::vars(dplyr::contains("pct")), function(x) x / 100) %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(
    count = count + 6,
    white_s = pctwhite * count + 1,
    black_s = pctblack * count + 1,
    hispanic_s = pcthispanic * count + 1,
    api_s = pctapi * count + 1,
    aian_s = pctaian * count + 1,
    other_s = pct2prace * count + 1,
    pr_other_s = other_s / count,
    pr_white_s = white_s / count,
    pr_black_s = black_s / count,
    pr_hispanic_s = hispanic_s / count,
    pr_api_s = api_s / count,
    pr_aian_s = aian_s / count,
    pr_s_white = white_s / sum(white_s),
    pr_s_black = black_s / sum(black_s),
    pr_s_hispanic = hispanic_s / sum(hispanic_s),
    pr_s_api = api_s / sum(api_s),
    pr_s_aian = aian_s / sum(aian_s),
    pr_s_other = other_s / sum(other_s)
  ) %>%
  dplyr::select(last_name = name, dplyr::contains("pr_"))

rm(surnames_2010)


# Geo-locations -----------------------------------------------------------

zips <- readr::read_csv("data-raw/nhgis_zip.csv") %>%
  dplyr::mutate(
    total_population = H7Z001 + 6,
    white_g = H7Z003 + 1,
    black_g = H7Z004 + 1,
    hispanic_g = H7Z010 + 1,
    api_g = H7Z006 + H7Z007 + 1,
    aian_g = H7Z005 + 1,
    other_g = H7Z009 + 1,
    pr_white_g = white_g / total_population,
    pr_black_g = black_g / total_population,
    pr_hispanic_g = hispanic_g / total_population,
    pr_api_g = api_g / total_population,
    pr_aian_g = aian_g / total_population,
    pr_other_g = other_g / total_population,
    pr_g_white = white_g / sum(white_g),
    pr_g_black = black_g / sum(black_g),
    pr_g_hispanic = hispanic_g / sum(hispanic_g),
    pr_g_api = api_g / sum(api_g),
    pr_g_aian = aian_g / sum(aian_g),
    pr_g_other = other_g / sum(other_g),
    zip = as.character(ZCTA5A)
  ) %>%
  dplyr::select(zip, dplyr::contains("pr_"))


blocks <- vroom::vroom("data-raw/nhgis_2010_blocks.csv") %>%
  dplyr::filter(H7Z001 > 0) %>%
  dplyr::mutate(
    total_population = H7Z001 + 6,
    white_g = H7Z003 + 1,
    black_g = H7Z004 + 1,
    hispanic_g = H7Z010 + 1,
    api_g = H7Z006 + H7Z007 + 1,
    aian_g = H7Z005 + 1,
    other_g = H7Z009 + 1,
    pr_white_g = white_g / total_population,
    pr_black_g = black_g / total_population,
    pr_hispanic_g = hispanic_g / total_population,
    pr_api_g = api_g / total_population,
    pr_aian_g = aian_g / total_population,
    pr_other_g = other_g / total_population,
    pr_g_white = white_g / sum(white_g),
    pr_g_black = black_g / sum(black_g),
    pr_g_hispanic = hispanic_g / sum(hispanic_g),
    pr_g_api = api_g / sum(api_g),
    pr_g_aian = aian_g / sum(aian_g),
    pr_g_other = other_g / sum(other_g),
    block = paste0(
      as.character(STATEA),
      as.character(COUNTYA),
      as.character(TRACTA),
      as.character(BLOCKA)
    )
  ) %>%
  dplyr::select(block, dplyr::contains("pr_"))

# Party -------------------------------------------------------------------

parties <- readr::read_csv("data-raw/parties.csv") %>%
  dplyr::select(party = PARTY, dplyr::everything())


# Apartment ---------------------------------------------------------------

units <- as.numeric(readr::read_csv("data-raw/nhgis_2000_units.csv")[6:165])
units <- c(units, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1)
units <- matrix(units, nrow = 20, ncol = 9)
colnames(units) <- c(
  "total_population",
  "hispanic_a",
  "white_a",
  "black_a",
  "aian_a",
  "asian_a",
  "nhpi_a",
  "other_a",
  "apartment"
)
apartments <- as.data.frame(units) %>%
  dplyr::group_by(apartment) %>%
  dplyr::summarise_all(sum) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    apartment = apartment,
    api_a = asian_a + nhpi_a,
    pr_white_a = white_a / total_population,
    pr_black_a = black_a / total_population,
    pr_hispanic_a = hispanic_a / total_population,
    pr_aian_a = aian_a / total_population,
    pr_api_a = api_a / total_population,
    pr_other_a = other_a / total_population,
    pr_a_white = white_a / sum(white_a),
    pr_a_black = black_a / sum(black_a),
    pr_a_hispanic = hispanic_a / sum(hispanic_a),
    pr_a_aian = aian_a / sum(aian_a),
    pr_a_api = api_a / sum(api_a),
    pr_a_other = other_a / sum(other_a)
  ) %>%
  dplyr::select(apartment, dplyr::contains("pr_"))


# Demographics ------------------------------------------------------------

age_gender <-
  as.numeric(readr::read_csv("data-raw/nhgis_2010_age_gender.csv")[6:1677])
age_gender <- c(age_gender, c((rep(0, 105)), rep(1, 105)))
age_gender <- matrix(age_gender, nrow = 209, ncol = 9)
colnames(age_gender) <-
  c(
    "total_population",
    "hispanic",
    "white",
    "black",
    "aian",
    "asian",
    "nhpi",
    "other",
    "female"
  )

genders <- as.data.frame(age_gender[-1, ]) %>%
  dplyr::group_by(female) %>%
  dplyr::summarise_all(sum) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    female = female,
    api = asian + nhpi,
    pr_white_fem = white / total_population,
    pr_black_fem = black / total_population,
    pr_hispanic_fem = hispanic / total_population,
    pr_aian_fem = aian / total_population,
    pr_api_fem = api / total_population,
    pr_other_fem = other / total_population,
    pr_fem_white = white / sum(white),
    pr_fem_black = black / sum(black),
    pr_fem_hispanic = hispanic / sum(hispanic),
    pr_fem_aian = aian / sum(aian),
    pr_fem_api = api / sum(api),
    pr_fem_other = other / sum(other)
  ) %>%
  dplyr::select(female, dplyr::contains("pr_"))


men <- age_gender[-c(1, 2, 106:209), -9]
women <- age_gender[-c(1:106), -9]

birth_years <- as.data.frame(men + women) %>%
  dplyr::slice(-(1:3)) %>%
  dplyr::mutate(birth_year = seq(2010, 1911),
         api = asian + nhpi,
         pr_white_y = white / total_population,
         pr_black_y = black / total_population,
         pr_hispanic_y = hispanic / total_population,
         pr_aian_y = aian / total_population,
         pr_api_y = api / total_population,
         pr_other_y = other / total_population,
         pr_y_white = white / sum(white),
         pr_y_black = black / sum(black),
         pr_y_hispanic = hispanic / sum(hispanic),
         pr_y_aian = aian / sum(aian),
         pr_y_api = api / sum(api),
         pr_y_other = other / sum(other)
  ) %>%
  dplyr::select(birth_year, dplyr::contains("pr_"))

rm(age_gender, units, men, women)




# Save the Data -----------------------------------------------------------

apartments <- as.data.frame(apartments)
birth_years <- as.data.frame(birth_years)
blocks <- as.data.frame(blocks)
firstnames <- as.data.frame(firstnames)
genders <- as.data.frame(genders)
parties <- as.data.frame(parties)
surnames <- as.data.frame(surnames)
zips <- as.data.frame(zips)

usethis::use_data(
  apartments,
  birth_years,
  blocks,
  firstnames,
  genders,
  parties,
  surnames,
  zips,
  overwrite = T,
  internal = T,
  compress = "xz"
)

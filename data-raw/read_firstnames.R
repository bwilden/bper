
first_names <- readxl::read_xlsx(here::here("data-raw", "firstnames.xlsx"),
                                 sheet = "Data")

usethis::use_data(first_names, overwrite = TRUE, internal = TRUE)

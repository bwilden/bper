#' Example Persons Data Set
#'
#' All individuals, except the first, are randomly generated from the ethnorace
#' distribution data sets.
#'
#' @export
example_persons <- data.frame(
  first_name = c("BERT", "LYNDON", "BELINDA", "ANNA", "KARL", "MATHIEU", "LIAM", "KAI", "PAMELLA"),
  last_name = c("WILDEN", "WITHER", "LOBOS", "ARENA", "SOM", "TURA", "SZYMONIAK", "WALKO", "CHANEL"),
  birth_year = c(1992, 1963, 1989, 1920, 1978, 1913, 1932, 1990, NA_integer_),
  female = c(0, 0, 1, 1, 0, 0, 1, 1, 1),
  party = c("DEM", "DEM", "DEM", "REP", "UNA", NA_character_, "REP", "UNA", "DEM"),
  apartment = c(1, 0, 1, 1, 1, 1, 0, NA_integer_, 0),
  state = c("CA", "VA", "FL", "HI", "TX", "NC", "MI", "CA", NA_character_),
  county = c("06073", "51173", "12009", NA_character_, "48073", "37097", "26103", "06025", NA_character_),
  zip = c("92092", "53146", "57551", "92844", "03862", "65557", "59730", NA_character_, NA_character_),
  block = c(
    "060730083052007",
    "511730301002001",
    "120090647001042",
    NA_character_,
    "480739503004014",
    "370970614081008",
    "261030022003121",
    NA_character_,
    NA_character_
  )
)

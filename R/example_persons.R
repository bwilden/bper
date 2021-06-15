#' Example Persons Data Set
#'
#' All individuals, except the first row, are randomly generated from the
#' ethnorace distribution data sets.
#'
#' @export
example_persons <- tibble(
  first_name = c("BERT", "LYNDON", "BELINDA", "ANNA",
                 "KARL", "MATHIEU", "LIAM", "KAI", "PAMELLA"),
  last_name = c("WILDEN", "WITHER", "LOBOS", "ARENA",
                "SOM", "TURA", "SZYMONIAK", "WALKO", "CHANEL"),
  age = c(28, 30, 33, 90, 13, 50, 20, 78, 65),
  sex = c(0, 0, 1, 1, 0, 0, 1, 1, 1),
  party = c("DEM", "DEM", "DEM", "REP", "UNA", "DEM", "REP", "UNA", "DEM"),
  multi_unit = c(1, 0, 1, 1, 1, 1, 0, 1, 0),
  state = c("CA", "VA", "FL", "HI", "TX", "NC", "MI", "CA", "NC"))


# The opposite of %in%
`%notin%` <- Negate(`%in%`)

# Product function with na.rm = TRUE as default
.prod <- purrr::partial(prod, na.rm = TRUE)

# Sys.setenv(CENSUS_KEY = "5e4c2b8438222753a7f4753fa78855eca73b9950")


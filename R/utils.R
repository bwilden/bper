
# The opposite of %in%
`%notin%` <- Negate(`%in%`)

# Product function with na.rm = TRUE as default
.prod <- purrr::partial(prod, na.rm = TRUE)



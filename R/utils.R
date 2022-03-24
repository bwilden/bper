
# The opposite of %in%
`%notin%` <- Negate(`%in%`)

# Product function with na.rm = TRUE as default
.rowProds <- purrr::partial(matrixStats::rowProds, na.rm = TRUE)



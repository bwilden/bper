
arg_max_cols <- function(df, num_cols, max_col_name = "max_col") {
  cols <- df[utils::tail(seq_along(df), num_cols)]
  max_col <- colnames(cols)[max.col(cols)]
  df[[max_col_name]] <- max_col

  return(df)
}

`%notin%` <- Negate(`%in%`)

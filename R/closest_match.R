#' n Closest Match Function
#'
#' This function match n closest distance from a numeric variable.
#' If the choosen variable rank is the same as others, they are shuffled
#' and randonly selected.
#' @inheritParams lift_func
#' @param var_1 numeric number to pivot on
#' @keywords closest_match
#' @return dataframe with n rows that have the closest match
#' @examples
#' closest_match(df, 0.5, "variable", 3)
#' @export

closest_match <- function(df, var_1, var_2, n) {
  sort_ls <- sort(abs(var_1 - df[, var_2]), method = "quick", index.return=TRUE)
  return(df[sample(sort_ls$ix[sort_ls$x %in% sort_ls$x[c(1:n)]])[c(1:n)],])
}

#' n Closest Match Function
#'
#' This function match n closest distance from a numeric variable
#' @param df data frame that contains all variables in it
#' @param var_1 numeric number to pivot on
#' @param var_lift column name in the dataframe to compared vector against `var_1`
#' @param n n closest variables
#' @keywords closest_match
#' @return dataframe with n rows that have the closest match
#' @examples
#' closest_match(df, 0.5, "variable", 3)

closest_match <- function(df, var_1, var_2, n) {
  sort_ls <- sort(abs(var_1 - df[, var_2]), method = "quick", index.return=TRUE)
  return(df[sort_ls$ix[c(1:n)],])
}

#' Lift Calculation Function
#'
#' This function calculate lift by comparing averages of nth closest pscore
#' @param df data frame that contains all variables in it
#' @param var_lift column name for the lift variable
#' @param var_lift_ind column name for the indicator variable
#' @param var_2 column name in the dataframe to compared vector against var_1
#' @param n nth closest variables
#' @param replacement with or without replacement of selection
#' @keywords lift_func
#' @return data frame with new lift column apended for indicator = 1 rows
#' @examples
#' lift_func(df, "NDS", "target", "pscore", 3)

lift_func <- function(df, var_lift, var_lift_ind, var_2, n, replacement = FALSE, ...) {
  replace.idx <- c()
  df_1 <- df[df[, var_lift_ind] == 1, ]
  df_0 <- df[df[, var_lift_ind] == 0, ]
  df_1[, paste0(var_lift, "_lift")] <- NA
  df_1[, "GUID_compared"] <- NA
  df_1[, "Abs Distance"] <- NA
  for(i in 1:nrow(df_1)) {
    temp <- closest_match(if(replacement == FALSE){df_0} else {df_0[-replace.idx, ]},
                          df_1[i, var_2], 
                          var_2, 
                          n)
    df_1[i, paste0(var_lift, "_lift")] <- df_1[i, var_lift] - mean(temp[, var_lift])
    df_1[i, "GUID_compared"] <- paste(temp[,"GUID"], collapse = ", ")
    df_1[i, "Abs Distance"] <- paste0("[", min(abs(df_1[i, var_2] - temp[, var_2])), ", ", 
                                      max(abs(df_1[i, var_2] - temp[, var_2])), "]")
    replace.idx <- c(replace.idx, as.numeric(rownames(temp)))
  }
  return(df_1)
}

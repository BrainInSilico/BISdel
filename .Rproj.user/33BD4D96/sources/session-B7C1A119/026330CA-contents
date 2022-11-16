
#' @description create a contingency table for 2 groups with n factors
#' @param x first group
#' @param y second group
#' @return matrix of contingencies with the group in lines and the factors in columns
contingencyTable2groups <- function(x, y) {
  possibleFactors <- as.factor(c(x,y))
  possibleFactors <- levels(possibleFactors)
  df <- data.frame(x, y)
  df <- gather(df, group, value, x:y, factor_key = T)
  return(table(df))
}

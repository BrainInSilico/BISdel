
addMissingFactor <-

#' @description create a contingency table for 2 groups with n factors
#' @param x first group
#' @param y second group
#' @return matrix of contingencies with the group in lines and the factors in columns
contingencyTable2groups <- function(x, y) {
  # identify factors existing in all groups
  possibleFactors <- as.factor(c(x,y))
  possibleFactors <- levels(possibleFactors)
  # count factors in each groups
  countX <- table(x)
  countY <- table(y)
  # add count 0 for factor in Y but not in X
  if(any(!possibleFactors %in% names(countX))) {
    missingFact <- possibleFactors[which(!possibleFactors %in% names(countX))]
    oldNames <- names(countX)
    countX <- c(countX, rep(0, length(missingFact)))
    names(countX) <- c(oldNames, missingFact)
  }
  # add count 0 for factor in X but not in Y
  if(any(!possibleFactors %in% names(countY))) {
    missingFact <- possibleFactors[which(!possibleFactors %in% names(countY))]
    oldNames <- names(countY)
    countY <- c(countY, rep(0, length(missingFact)))
    names(countY) <- c(oldNames, missingFact)
  }
  # reorder countX and countY by factor names
  countX <- countX[order(names(countX))]
  countY <- countY[order(names(countY))]
  return(rbind(countX, countY))
}

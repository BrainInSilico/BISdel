


#' @description calculate correlation of different types (continious, binary, nominal, ordinal) for 2 vectors
#' @param x vector of data
#' @param y another vector of data of the same type than x
#' @param method continious, binary, nominal or ordinal
#' @return a correlation value between -1 and 1
correlationByType <- function(x, y, method=c('continious', 'binary', 'nominal', 'ordinal')) {
  method <- tolower(method)
  if(! method %in% c('continious', 'binary', 'nominal', 'ordinal')) {
    stop(paste('method', method, 'not recognized, please use one of : continious, binary, nominal or ordinal'))
  }
  nbLevel <- NULL
  if(method == 'ordinal') {
    nbLevel <- as.factor(c(x,y))
    nbLevel <- length(levels(nbLevel))
  }
  if(method != 'continious') {
    df <- contingencyTable2groups(x, y)
  }
  corr <- switch(method,
                 continious = cor(x, y, use = 'pairwise.complete.obs'),
                 binary = tetrachoric(df)$rho,
                 ordinal = polycor::polychor(x,y),
                 nominal = cramerV(df)
                 )
  return(corr)
}

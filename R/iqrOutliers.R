#' IQR Outliers
#'
#' This function takes a vector, and replace observations that are smaller or bigger than the lower/upper bounds with NA.
#'
#' @param data Input vector
#' @param lowerBound Lower bound (default value is 2.5)
#' @param upperBound Upper bound (default value is 2.5)
#' @param replace If replace is TRUE (default is FALSE) than the function also use the replaceWith function with the median default values
#'
#'
#' @return A vector with outliers as NA values
#' @export
#'
#' @examples iqrOutliers(data = simulateData$Age)
iqrOutliers <- function(data, lowerBound = 2.5, upperBound = 2.5, replace = FALSE){

  Q      <- stats::quantile(data, probs = c(.25, .75), na.rm = TRUE)
  iqr    <- stats::IQR(data, na.rm = TRUE)
  High   <- Q[2] + upperBound * iqr
  Low    <- Q[1] - lowerBound * iqr
  Final  <- ifelse(data > High | data < Low, NA, data)

  if(replace == TRUE){
    Final <- replaceWith(data = Final, type = 'Median', Ratio = 0.8)
  }

  return(Final)
}

#' SD (standard deviation) outliers
#'
#' This function takes a vector, and replace observations that are smaller or bigger than the Mean + the lower/upper bounds * standard deviation with NA.
#'
#' @param data A numeric vector
#' @param lowerBound Lower bound standard deviation (default value is 3)
#' @param upperBound Upper bound standard deviation (default value is 3)
#' @param replace If replace is TRUE (default is FALSE) than the function also use the replaceWith function with the default values
#'
#' @return A vector with outliers as NA values
#' @export
#'
#' @examples sdOutliers(theData$Score)
#'
sdOutliers <- function(data, lowerBound = 3, upperBound = 3, replace = FALSE){

  Mean   <- mean(data, na.rm = TRUE)
  SD     <- stats::sd(data, na.rm = TRUE)
  High   <- Mean + upperBound * SD
  Low    <- Mean - lowerBound * SD
  Final  <- ifelse(data > High | data < Low, NA, data)

  if(replace == TRUE){
    Final <- replaceWith(Final)
  }

  return(Final)

}

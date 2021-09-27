#' Package's data calculation
#' Calculating  observations' ID, Age, Gender, Condition in A/B/c testing AND score.
#'
#' @return A data frame
#' @export
#'
#' @examples Data <- packageData()
#'
packageData <- function(){

  set.seed(123)
  ID <- rep(c(1:10), 3)
  Age <- round(rnorm(30, 50, 10), 2)
  Error <- rnorm(30, 5, 1)
  Score <- round(Age * 1.5 - Error, 2)
  Gender <- rep(rep(c('M', 'F'), 5), 3)
  Condition <- c(rep('A', 10), rep('B', 10), rep('C', 10))

  Data <- data.frame(ID, Age, Gender, Condition, Score)
  return(Data)
}




#' Package's data calculation
#' Calculating  observations' ID, Age, Gender, Condition in A/B/c testing AND score.
#'
#' @return A data frame
#' @export
#'
#' @examples Data <- packageData()
#'


packageData <- function(){

  ID           <- rep(c(1:100), c(101:200), 2)
  set.seed(123)
  Age          <- rep(c(rnorm(100, 50, 10), rnorm(100, 45, 10)), 2)
  Gender       <- rep(c('Male', 100), c('Female', 100), 2)
  Error        <- rnorm(50, 1, 1)
  Error2       <- rnorm(50, 10, 10)
  gameTime     <- ifelse(Gender == 'Male' & Age < 30, 60 + sample(Error, 1),
                         ifelse(Gender == 'Male', Age + sample(Error, 1), Age + sample(Error, 1)))
  Score        <- Age + sample(Error2, 1)
  Age[190:200] <- NA
  Time         <- c(rep('T1', 100), rep('T2', 100), rep('T3', 100), rep('T4', 100))
  Data         <- data.frame(ID, Age, Gender, Time, gameTime, Score)

  return(Data)

}

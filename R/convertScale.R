#' Convert Scale
#'
#' This function convert to a numeric vector an ordinal vector with which composed from a string.
#'
#' @param data A vector as data.
#' @param Levels A vector with the levels by their order.
#' @param Reverse Boolean (default is FALSE) indicates whether to revers the scale.
#'
#' @return A numeric vector
#' @export
#'
#' @examples convertSacle(theData$Condition, c('A', 'B', 'C'))
#'
convertSacle <- function(data, Levels, Reverse = FALSE){

  Length <- length(Levels)
  data <- as.character(data)

  i <- 1
  while (i <= Length){
    data[data == Levels[i]] <- i
    i <- i + 1
  }

  data <- as.numeric(data)

  if(Reverse == TRUE){
    data <- Length + 1 - data
  }

  return(data)
}

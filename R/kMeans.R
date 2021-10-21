#' k-Means
#'
#' The function computes the optimal k-means.
#' Based on stats::kmeans()
#'
#'
#' @param data A vector or a data frame which contains the variables for the K-Means calculation
#' @param nStart The minimum number of groups (default is 2)
#' @param nLimit The maximum number of groups (default is 15)
#' @param varExp The final increased variance value for best model (default = 0.05 which is 5%)
#' @param Seed A Seed for randomization (default is 123)
#'
#' @return The function returns a list with the following values values
#' @return Clusters: Each observation group
#' @return Means: The groups' means
#' @return Ns: The groups' Ns
#' @return Vrainace_Explained: The explained variance
#' @export
#'
#' @examples kMeans(data = simulateData$Age)
#' @examples kMeans(data = cbind(simulateData$Age, simulateData$Score))
#'
kMeans <- function(data, nStart = 2, nLimit = 15, varExp = 0.05, Seed = 123){

  if(sum(as.numeric(is.na(data) == TRUE)) > 0){

    print('Warning: There are missing values in the data set, please remove it.')

  }

  set.seed(Seed)
  Model1 <- stats::kmeans(x = data, centers = nStart)
  nStart <- nStart + 1
  Round  <- 1
  End    <- 'No'

  while(End == 'No' | Round <= nLimit){

    set.seed(Seed)
    Model2 <- stats::kmeans(x = data, centers = nStart)
    End    <- ifelse(Model2$betweenss / Model2$totss - Model1$betweenss / Model1$totss >= varExp,
                  End, 'Yes')

    if(End == 'No'){
      set.seed(Seed)
      Model1 <- stats::kmeans(x = data, centers = nStart)
    }

    nStart <- nStart + 1
    Round  <- Round + 1
  }


  Answer <- list(Clusters = Model1$cluster, Means = round(Model1$centers, 2),
                 Ns = Model1$size, Vrainace_Explained = round(Model1$betweenss / Model1$totss, 2))

  return(Answer)
}

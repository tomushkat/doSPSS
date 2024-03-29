% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kMeans.R
\name{kMeans}
\alias{kMeans}
\title{k-Means}
\usage{
kMeans(data, nStart = 2, nLimit = 15, varExp = 0.05, Seed = 123)
}
\arguments{
\item{data}{A vector or a data frame which contains the variables for the K-Means calculation}

\item{nStart}{The minimum number of groups (default is 2)}

\item{nLimit}{The maximum number of groups (default is 15)}

\item{varExp}{The final increased variance value for best model (default = 0.05 which is 5\%)}

\item{Seed}{A Seed for randomization (default is 123)}
}
\value{
The function returns a list with the following values values

Clusters: Each observation group

Means: The groups' means

Ns: The groups' Ns

Vrainace_Explained: The explained variance
}
\description{
The function computes the optimal k-means.
Based on stats::kmeans()
}
\examples{
kMeans(data = simulateData$Age)
kMeans(data = cbind(simulateData$Age, simulateData$Score))

}

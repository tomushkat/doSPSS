% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convertScale.R
\name{convertSacle}
\alias{convertSacle}
\title{Convert Scale}
\usage{
convertSacle(data, Levels, Reverse = FALSE)
}
\arguments{
\item{data}{A vector as data.}

\item{Levels}{A vector with the levels by their order.}

\item{Reverse}{Boolean (default is FALSE) indicates whether to revers the scale.}
}
\value{
A numeric vector
}
\description{
This function convert to a numeric vector an ordinal vector with which composed from a string.
}
\examples{
convertSacle(data = simulateData$Condition, Levels = c('A', 'B'))

}

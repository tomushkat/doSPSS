% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rmAnova.R
\name{rmAnova}
\alias{rmAnova}
\title{Repeated measures One Way Anova}
\usage{
rmAnova(DV, IDV, Within, Parametric = TRUE, Correct = "BH")
}
\arguments{
\item{DV}{A vector with the dependent variable}

\item{IDV}{A vector with the independent variable}

\item{Within}{The identity of the cases / The identity of the within factor}

\item{Parametric}{If FALSE the test is Friedman test with Wilcoxon Signed Rank tests for post hoc pairwise comparisons}

\item{Correct}{The type of correction for post hoc (default is Benjamini, Y., and Hochberg, Y. (1995)) for more details go to the function pairwise.t.test}
}
\value{
A list with the following components:

Descriptive_statistics: Descriptive statistics with the Mean, standard deviation, Median and N

Model_summary: Model's summary

Effect_size: eta square effect size (if the model is significant and parametric)

Post_hoc: Pairwise comparisons (if the model is significant)

Figure
}
\description{
This function conducts repeated measures ANOVA. If the model is significant, an effect size and pairwise comparisons also be produced. The data set should be with exact n observations per ID, otherwise the figure's and descriptive statistics' results will be inaccurate
Based on stats::aov()
}
\examples{
rmAnova(DV = simulateData$gameTime, IDV = simulateData$measureTime, Within = simulateData$ID)

}

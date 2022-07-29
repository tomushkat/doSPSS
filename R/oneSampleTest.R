#' One sample t.test
#'
#' This function conducts a one sample t.test or one sample Mann-Whitney.
#'
#' @param DV TA vector with the dependent variable
#' @param MU The parameter for comparison
#' @param Parametric If FALSE the test is Mann-Whitney test
#'
#' @return A list with the following components:
#' @return Descriptive_statistics: Descriptive statistics with the Mean, standard deviation, Median and N
#' @return Model_summary: Model's summary
#' @return Effect_size: Cohen's d/rank_biserial effect size (if the model is significant and parametric)
#' @return Figure
#' @export
#'
#' @examples oneSampleTest(DV = simulateData$Score, MU = 5)
oneSampleTest <- function (DV, MU = 0, Parametric = TRUE) {


  Data <- data.frame(DV) %>%
    tidyr::drop_na()

  Statistics <- Data %>%
    dplyr::summarise(
      Mean   = round(mean(DV), 2),
      SD     = round(stats::sd(DV), 2),
      #Median = round(stats::median(DV), 2),
      N      = length(DV)
    )

  EF <- "No effect size for insignificant results"
  if (Parametric == TRUE) {

    Model <- stats::t.test( DV, mu = MU, alternative = "two.sided")

    if (Model$p.value < .05) {

      EF <- effectsize::effectsize(model = Model,
                                   type = 'cohens_d', ci = .95,
                                   alternative = "two.sided")
    }
    if (nrow(Data < 30)) {

      Shapiro <- shapiro.test(DV)

      if(Shapiro$p.value < .05){

        print(c("Warning: There are fewer than 30 observations, and the DVs distribution is not normal. Considre to use an kruskal-wallis test (Parametric = FALSE)"))

      }
    }
  } else {

    Model <- stats::wilcox.test(DV, mu = MU, alternative = "two.sided")

    if (Model$p.value < 0.05) {

      EF <- effectsize::rank_biserial(DV,
                                      mu = MU,
                                      ci = 0.95,
                                      alternative = "two.sided",
                                      verbose = TRUE)
    }

  }


  Figure <-
    Data %>%
    ggplot2::ggplot(ggplot2::aes(x = DV)) +
    ggplot2::geom_histogram() +
    ggplot2::geom_vline(xintercept = Statistics$Mean, linetype="dotted",
                        color = "red", size=1.5) +
    ggplot2::geom_vline(xintercept = Statistics$Med, linetype="dashed",
                        color = "red", size=1.5) +
    ggplot2::geom_vline(xintercept = c(Statistics$Mean + Statistics$SD), linetype="dotted",
                        color = "blue", size=1.5) +
    ggplot2::geom_vline(xintercept = c(Statistics$Mean - Statistics$SD), linetype="dotted",
                        color = "blue", size=1.5) +
    ggplot2::theme_bw()


  return(list(Statistics, Model, EF, Figure))


}

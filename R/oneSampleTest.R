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
oneSampleTest <- function(DV, MU = 0, Parametric = TRUE) {


  Data <- data.frame(DV) %>%
    tidyr::drop_na()

  Statistics <- Data %>%
    dplyr::summarise(
      Mean   = round(mean(DV), 2),
      SD     = round(stats::sd(DV), 2),
      Median = stats::median(DV),
      N      = length(DV)
    )

  EF <- "No effect size for insignificant results"
  EF_exp <- NULL
  if (Parametric) {

    Model <- stats::t.test(DV, mu = MU, alternative = "two.sided")

    if (Model$p.value < .05) {

      EF <- effectsize::cohens_d(DV, mu = MU, alternative = 'two.sided', ci = 0.95)
      EF_1 <- abs(unlist(EF$Cohens_d))
      EF_value <- ifelse(EF_1 < 0.3, 'less than small effect size.',
                         ifelse(EF_1 >= 0.3 & EF_1 < 0.5, 'smaall effect size.',
                                ifelse(EF_1 >= 0.5 & EF_1 < 0.8, 'medium effect size.',
                                       ifelse(EF_1 >= 0.8, 'large effect size.', NA))))
      EF_exp <- paste0("The Cohen's d value is ", round(EF$Cohens_d, 2), ', which is interpreted as a ', EF_value)

    }
    if (nrow(Data) < 30) {

      Shapiro <- stats::shapiro.test(DV)

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
      EF_value <- ifelse(abs(EF$r_rank_biserial) < 0.1, 'less than small effect size.',
                         ifelse(abs(EF$r_rank_biserial) >= 0.1 & abs(EF$r_rank_biserial) < 0.3, 'small effect size.',
                                ifelse(abs(EF$r_rank_biserial) >= 0.3 & abs(EF$r_rank_biserial) < 0.5, 'medium effect size.',
                                       ifelse(abs(EF$r_rank_biserial) >= 0.5, 'large effect size.', NA))))
      EF_exp <- paste0('The effect size value is ', round(EF$r_rank_biserial, 2), ' which is interpreted as a', EF_value)
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


  return(list(Descriptive_statistics = Statistics, Model_summary = Model, Effect_size = EF, Effect_interpretation = EF_exp, Figure = Figure))


}

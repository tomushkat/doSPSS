#' Independent samples t-test
#'
#' The function uses the 'stats' package to conduct the independent sample t-test or the Welch test
#' The correction for the Welch test is automatically using the Leven test
#' Based on stats::t.test()
#'
#'
#' @param DV A vector with the dependent variable
#' @param IDV A vector with the independent variable
#' @param Parametric If FALSE the test is Mann-Whitney test
#'
#' @return A list with the following components:
#' @return Descriptive_statistics: Descriptive statistics with the Mean, standard deviation, Median and N
#' @return Model_summary: Model's summary (with or without correction for variance)
#' @return Effect_size: Cohen's d effect size (if the model is significant and parametric)
#' @return Variance_Correction: If TRUE a correction for the variance was conducted (the test type is 'Welch's t.test')
#' @return Figure
#' @export
#'
#' @examples indttest(theData$Score, theData$Gender)
#'
#'
indttest <- function(DV, IDV, Parametric = TRUE){

  Data <- data.frame(DV, IDV)
  Data <- Data[stats::complete.cases(Data), ]

  Statistics <- Data %>%
    dplyr::group_by(IDV) %>%
    dplyr::summarise(
      Mean   = round(mean(DV), 2),
      SD     = round(stats::sd(DV), 2),
      Median = round(stats::median(DV), 2),
      N      = length(DV)
    )

  if(Parametric == TRUE){
    varTest     <- stats::var.test(formula = DV ~ IDV, data = Data)
    trueVarTest <- ifelse(varTest$p.value > 0.05, TRUE, FALSE)
    Model       <- stats::t.test(formula = DV ~ IDV, data = Data,
                                 var.equal = trueVarTest, alternative = "two.sided", mu = 0, paired = FALSE, conf.level = 0.95)

    if(Model$p.value < 0.05){

      EF <- effectsize::effectsize(model = Model,
                                   type = 'cohens_d', ci = .95, alternative = "two.sided")

    }else{EF <- NULL}

  }else{

    Model <- stats::wilcox.test(formula = DV ~ IDV, data = Data,
                                paired = FALSE, alternative = "two.sided", exact = NULL, mu = 0, correct = FALSE,
                                conf.int = FALSE, conf.level = 0.95)
    EF <- NULL

  }

  Figure <-
    ggplot2::ggplot(Data, ggplot2::aes(x = IDV, y = DV, fill = IDV)) +
    ggplot2::geom_boxplot(color = 'purple', alpha = 2) +
    ggplot2::geom_violin(alpha = 0.1) +
    ggplot2::scale_fill_manual(values = c('lightgrey', 'lightgreen')) +
    # ggplot2::geom_jitter(ggplot2::aes(x = IDV, y = DV, fill = IDV)) +
    ggplot2::stat_summary(fun.data = ggplot2::mean_sdl, fun.args = list(mult = 1),
                          geom = "errorbar", color = "red", width = 0.2) +
    ggplot2::stat_summary(fun = mean, geom = "point", color = "red") +
    ggplot2::ylab('DV') + ggplot2::xlab('IDV') +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) + ggplot2::theme_bw()

  L <- list(Descriptive_statistics = Statistics, Model_summary = Model, Effect_size = EF, Variance_Correction = !trueVarTest, Figure = Figure)

  return(L)

}

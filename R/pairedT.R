#' Paired samples t-test
#'
#' The function conducts paired sample t-test. The data set should be with exact n observations per ID, otherwise the figure's and descriptive statistics' results will be inaccurate.
#' Based on stats::t.test()
#'
#' @param Within The identity of the cases / The identity of the within factor
#' @param DV A vector with the dependent variable
#' @param IDV A vector with the independent variable
#' @param Parametric If FALSE the test is Wilcoxon Signed Rank test
#'
#' @return A list with the following components:
#' @return Descriptive_statistics: Descriptive statistics with the Mean, standard deviation, Median and N
#' @return Model_summary: Model's summary
#' @return Effect_size: Cohen's d effect size (if the model is significant and parametric)
#' @return Figure
#' @export
#'
#' @examples pairedT(DV = theData$Score, IDV = theData$Gender, Within = theData$ID)
#'
pairedT <- function(DV, IDV, Within, Parametric = TRUE){

  Data <- data.frame(DV, IDV, Within)
  Data <- Data[stats::complete.cases(Data), ]
  Data <- Data %>%
    dplyr::arrange(Within, IDV)   # Arranging the data by the ID and the IDV

  Statistics <- Data %>%
    dplyr::group_by(IDV) %>%
    dplyr::summarise(
      Mean   = round(mean(DV), 2),
      SD     = round(stats::sd(DV), 2),
      Median = round(stats::median(DV), 2),
      N      = length(DV)
    )


  if(Parametric == TRUE){   # If the model is parametric

    Model <- Model       <- stats::t.test(formula = DV ~ IDV, data = Data,
                                          alternative = "two.sided", mu = 0, paired = TRUE, conf.level = 0.95)  # Performing a paired t-test

    if(Model$p.value < 0.05){   # If the model is significant

      EF <- effectsize::effectsize(model = Model,
                                   type = 'cohens_d', ci = .95, alternative = "two.sided")  # Performing effect size

    }else{EF <- NULL}

  }else{  # If the model is A-parametric

    Model <- stats::wilcox.test(formula = DV ~ IDV, data = Data,                        # Performing Wilcoxon
                                paired = TRUE, alternative = "two.sided", exact = NULL, mu = 0, correct = FALSE,
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

  L <- list(Descriptive_statistics = Statistics, Model_summary = Model, Effect_size = EF, Figure = Figure)

  return(L)

}

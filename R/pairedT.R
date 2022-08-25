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
#' @examples pairedT(DV = simulateData$Score, IDV = simulateData$Gender, Within = simulateData$ID)
#'
pairedT <- function(DV, IDV, Within, Parametric = TRUE){

  # Parameters for test
  # DV = simulateData$Score
  # IDV = simulateData$Gender
  # Within = simulateData$ID
  # Parametric = TRUE

  Data <- data.frame(DV, IDV, Within) %>%
    arrange(Within)
  # Data <- Data[stats::complete.cases(Data), ]


  Statistics <- Data %>%
    dplyr::group_by(IDV) %>%
    dplyr::summarise(
      Mean   = round(mean(DV), 2),
      SD     = round(stats::sd(DV), 2),
      Median = round(stats::median(DV), 2),
      N      = length(DV)
    )


  EF <- "No effect size for aparametric test or insignificant results"
  EF_exp <- NULL

  if (Parametric) {   # If the model is parametric

    Model <- stats::t.test(formula = DV ~ IDV, data = Data,
                           alternative = "two.sided", mu = 0,
                           paired = TRUE, conf.level = 0.95)  # Performing a paired t-test

    if (Model$p.value < 0.05) {   # If the model is significant

      EF <- effectsize::cohens_d(x = DV ~ IDV, data = Data,
                                 paired = TRUE,  ci = .95,
                                 alternative = "two.sided")
      EF_1 <- abs(unlist(EF$Cohens_d))
      EF_value <- ifelse(EF_1 < 0.3, 'less than a small effect size.',
                         ifelse(EF_1 >= 0.3 & EF_1 < 0.5, 'a small effect size.',
                                ifelse(EF_1 >= 0.5 & EF_1 < 0.8, 'a medium effect size.',
                                       ifelse(EF_1 >= 0.8, 'a large effect size.', NA))))
      EF_exp <- paste0("The Cohen's d value is ", round(EF$Cohens_d, 2), ', which is interpreted as ', EF_value)

    }

  } else {  # If the model is A-parametric

    Model <- stats::wilcox.test(formula = DV ~ IDV, data = Data,                        # Performing Wilcoxon
                                paired = TRUE, alternative = "two.sided",
                                exact = NULL, mu = 0, correct = FALSE,
                                conf.int = FALSE, conf.level = 0.95)


    if (Model$p.value < 0.05) {   # If the model is significant

      EF <- effectsize::rank_biserial(DV ~ IDV, data = Data,
                                      paired = TRUE,
                                      mu = 0,
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

  Data$IDV <- as.factor(Data$IDV)
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

  L <- list(Descriptive_statistics = Statistics, Model_summary = Model, Effect_size = EF, Effect_interpretation = EF_exp, Figure = Figure)


  if (Parametric == TRUE & nrow(Data) < 60) {

    Data <- Data %>%
      dplyr::arrange(IDV, Within)   # Arranging the data by the ID and the IDV
    Res <- Data$DV[1:(nrow(Data) / 2)] - Data$DV[((nrow(Data) / 2) + 1):nrow(Data)]
    shapiroTest <- stats::shapiro.test(Res)

    if (shapiroTest$p.value < .05) {

      print(c("Warning: There are fewer than 30 observations, and the differences' distribution is not normal. Considre to use an Wilcoxon test (Parametric = FALSE)"))

    }
  }

  return(L)

}

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
#' @return Effect_size: Cohen's d/rank_biserial effect size (if the model is significant and parametric)
#' @return Variance_Correction: If TRUE a correction for the variance was conducted (the test type is 'Welch's t.test')
#' @return Figure
#' @export
#'
#' @examples indttest(DV = simulateData$Score, IDV = simulateData$Gender)
#'
#'
indttest <- function(DV, IDV, Parametric = TRUE){

  # Parameters for test
  # DV = simulateData$Score
  # IDV = simulateData$Gender
  # Parametric = TRUE

  Data <- data.frame(DV, IDV) %>%
    tidyr::drop_na()
  # Data <- Data[stats::complete.cases(Data), ]


  Statistics <- Data %>%
    dplyr::group_by(IDV) %>%
    dplyr::summarise(
      Mean      = round(mean(DV), 2)
      , SD      = round(stats::sd(DV), 2)
      , low_CI  = mean_CI(DV)$L
      , high_CI = mean_CI(DV)$H
      , Median  = round(stats::median(DV), 2)
      , N       = length(DV)
    )

  EF <- "No effect size for insignificant results"
  EF_exp <- NULL
  trueVarTest <- 'No variation equality neaded of aparametric test'

  if (Parametric) {

    varTest     <- stats::var.test(formula = DV ~ IDV, data = Data)
    trueVarTest <- ifelse(varTest$p.value < 0.05, FALSE, TRUE)
    Model       <- stats::t.test(formula = DV ~ IDV, data = Data,
                                 var.equal = trueVarTest, alternative = "two.sided",
                                 mu = 0, paired = FALSE, conf.level = 0.95)

    if (Model$p.value < 0.05) {

      EF <- effectsize::cohens_d(x = DV ~ IDV, data = Data,
                                 pooled_sd = !trueVarTest,  ci = .95,
                                   alternative = "two.sided")
      EF_1 <- abs(unlist(EF$Cohens_d))
      EF_value <- dplyr::case_when(

        EF_1 < 0.3 ~ 'less than a small effect size.'
        , EF_1 < 0.5 ~ 'small effect size.'
        , EF_1 < 0.8 ~ 'medium effect size.'
        , T ~ 'large effect size.'

        )

      EF_exp <- paste0("The Cohen's d value is ", round(EF$Cohens_d, 2), ', which is interpreted as a ', EF_value)
    }

  } else {

    Model <- stats::wilcox.test(formula = DV ~ IDV, data = Data,
                                paired = FALSE, alternative = "two.sided",
                                exact = NULL, mu = 0, correct = FALSE,
                                conf.int = FALSE, conf.level = 0.95)

    if (Model$p.value < 0.05) {

      EF <- effectsize::rank_biserial(DV ~ IDV, data = Data,
                                             mu = 0,
                                             ci = 0.95,
                                             alternative = "two.sided",
                                             verbose = TRUE)
      EF_value <- dplyr::case_when(

        abs(EF$r_rank_biserial) < 0.1 ~ 'less than a small effect size.'
        , abs(EF$r_rank_biserial) < 0.3 ~ 'small effect size.'
        , abs(EF$r_rank_biserial) < 0.5 ~ 'medium effect size.'
        , T ~ 'large effect size.'

      )


      EF_exp <- paste0('The rank biserial value is ', round(EF$r_rank_biserial, 2), ' which is interpreted as ', EF_value)
      # https://stats.stackexchange.com/questions/216283/how-to-interpret-rank-biserial-correlation-coefficients-for-wilcoxon-test
    }

  }

  Data$IDV <- as.factor(Data$IDV)
  Figure <-
    ggplot2::ggplot(Data, ggplot2::aes(x = IDV, y = DV, fill = IDV)) +
    # ggplot2::stat_summary(fun.data = "mean_cl_boot", colour = "red", linewidth = 2, size = 2) +
    ggplot2::geom_boxplot(color = 'purple') +
    ggplot2::geom_violin(alpha = 0.1) +
    ggplot2::scale_fill_manual(values = c('lightgrey', 'lightgreen')) +
    # ggplot2::geom_jitter(ggplot2::aes(x = IDV, y = DV, fill = IDV)) +
    ggplot2::stat_summary(fun.data = ggplot2::mean_sdl, fun.args = list(mult = 1),
                          geom = "errorbar", color = "red", width = 0.2) +
    ggplot2::stat_summary(fun = mean, geom = "point", color = "darkred", size = 5) +


    ggplot2::ylab('DV') + ggplot2::xlab('IDV') +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::theme(
          axis.text.x = ggplot2::element_text(size = 30),
          axis.text.y  = ggplot2::element_text(size = 30),
          axis.title.x = ggplot2::element_text(size = 30),
          axis.title.y = ggplot2::element_text(size = 30),
          legend.text  = ggplot2::element_text(size = 30),
          legend.title = ggplot2::element_text(size = 30)
    ) +
    ggplot2::theme_bw()

  L <- list(Descriptive_statistics = Statistics, Model_summary = Model, Effect_size = EF, Effect_interpretation = EF_exp, Variance_Equality = trueVarTest, Figure = Figure)

  freq <- table(IDV)
  if (Parametric == TRUE & sum(as.numeric(freq < 30)) != 0) {

    if (trueVarTest == FALSE) {

      lmModel <- stats::lm(formula = DV ~ IDV, data = Data)
      Res     <- lmModel$residuals

    } else {

      lmModel <- estimatr::lm_robust(formula = DV ~ IDV, se_type = 'HC2', data = Data)
      Res     <- Data$DV - lmModel$fitted.values

    }

    shapiroTest <- stats::shapiro.test(Res)

    if (shapiroTest$p.value < .05) {
      Warning <- c("Warning: At list one of the groups has fewer than 30 observations, and the residuals' distribution is not normal. Considre to use an Mann-Whitney test (Parametric = FALSE)")
      print(Warning)

    }
  }

  return(L)

}

#' One Way Anova
#'
#' This function conducts a type 3 one way ANOVA or with hc2 correction type (if the variances are unequal). If the model is significant, an effect size and pairwise comparisons  also produced
#' Based on stats::aov(), car::Anova()
#'
#' @param DV A vector with the dependent variable
#' @param IDV A vector with the independent variable
#' @param Parametric If FALSE the test is Kruskal-Wallis test with Mann-Whitney test for post hoc pairwise comparisons
#' @param Correct The type of correction for post hoc (default is Benjamini, Y., and Hochberg, Y. (1995)) for more details go to the function pairwise.t.test
#'
#'
#' @return A list with the following components:
#' @return Descriptive_statistics: Descriptive statistics with the Mean, standard deviation, Median and N
#' @return Model_summary: Model's summary (with or without correction for variance)
#' @return Effect_size: eta square effect size (if the model is significant and parametric)
#' @return Post_hoc: Pairwise comparisons (if the model is significant)
#' @return Variance_Correction: If TRUE a correction for the variance was conducted (the test type is 'Analysis of Deviance')
#' @return Figure
#' @export
#'
#' @examples oneWayAnova(DV = simulateData$Score, IDV = simulateData$Condition)
#'
oneWayAnova <- function(DV, IDV, Parametric = TRUE, Correct = 'BH'){

  # Parameters for test
  # DV = simulateData$Score
  # IDV = simulateData$Condition
  # Parametric = TRUE
  # Correct = 'BH'

  Data <- data.frame(DV, IDV) %>%
    tidyr::drop_na()
  # Data <- Data[stats::complete.cases(Data), ]
  Data <- Data %>%
    dplyr::mutate(IDV = as.factor(IDV))

  Statistics <- Data %>%
    dplyr::group_by(IDV) %>%
    dplyr::summarise(
      Mean   = round(mean(DV), 2),
      SD     = round(stats::sd(DV), 2),
      low_CI = mean_CI(DV)$L,
      high_CI = mean_CI(DV)$H,
      Median = round(stats::median(DV), 2),
      N      = length(DV)
    )


  EF       <- "No effect size for insignificant results"
  varLeven <- 'No variation equality neaded of aparametric test'
  PH       <- 'No post hoc analysis for insignificant results'
  EF_exp        <- NULL


  if(Parametric){                     # if the model is  parametric

    Leven       <- car::leveneTest(Data$DV ~ Data$IDV)           # Variance test
    varLeven    <- ifelse(Leven$`Pr(>F)`[1] < .05, TRUE, FALSE)  # If the variances are not equale than TRUE
    modelOneWay <- stats::aov(formula = DV ~ IDV, data = Data)   # Preforming the ANOVA with stats package, without correction
    Model       <- car::Anova(mod = modelOneWay, type = 'III', white.adjust = varLeven)  # Preforming the model with car package with correction (optional) and as type 3
    ModelForEF  <- car::Anova(mod = modelOneWay, type = 'III')
    EF_exp      <- NULL

    if(Model$`Pr(>F)`[2] < 0.05){    # If the model is significant

      PH <- postHoc(DV = Data$DV, IDV = Data$IDV, Paired = FALSE, Parametric = TRUE, Correction = Correct)  # Preform post hoc
      EF <- effectsize::eta_squared(ModelForEF, ci = .95, alternative = "two.sided")   # Perform effect size

      EF_value <- dplyr::case_when(

        abs(EF$Eta2) < 0.01 ~ 'less than a small effect size (agreement).'
        ,  abs(EF$Eta2) < 0.06 ~ 'a small effect size (agreement).'
        ,  abs(EF$Eta2) < 0.14 ~ 'a medium effect size (agreement).'
        , T ~ 'a large effect size (agreement).'

      )
      EF_exp <- paste0('The eta squared value is ', round(EF$Eta2, 2), ' which is interpreted as a', EF_value)

    }

  }else{

    Model <- stats::kruskal.test(formula = DV ~ IDV, data = Data)    # if not parametric, perform kruskal wallis

    if(Model$p.value < 0.05){  # If the model is significant

      PH <- postHoc(DV = Data$DV, IDV = Data$IDV, Paired = FALSE, Parametric = FALSE, Correction = Correct)  # Perform post hoc

      EF <- effectsize::rank_epsilon_squared(DV ~ IDV, data = Data,
                                       mu = 0,
                                       ci = 0.95,
                                       alternative = "two.sided",
                                       verbose = TRUE)
      EF_value <- dplyr::case_when(

        abs(EF$rank_epsilon_squared) < 0.01 ~ 'less than a small effect size (agreement).'
        ,  abs(EF$rank_epsilon_squared) < 0.04 ~ 'a small effect size (agreement).'
        ,  abs(EF$rank_epsilon_squared) < 0.16 ~ 'a medium effect size (agreement).'
        , T ~ 'a large effect size (agreement).'

      )

      EF_exp <- paste0('The rank epsilon squared value is ', round(EF$rank_epsilon_squared, 2), ' which is interpreted as a', EF_value)



    }
  }

  Data$IDV <- as.factor(Data$IDV)
  Figure <- ggplot2::ggplot(Data, mapping = ggplot2::aes(x = IDV, y = DV, fill = IDV)) +
    ggplot2::geom_boxplot(color = 'purple', alpha = 2) +
    ggplot2::geom_violin(alpha = 0.1) +
    # ggplot2::geom_jitter(ggplot2::aes(x = IDV, y = DV, fill = IDV)) +
    ggplot2::stat_summary(fun.data = ggplot2::mean_sdl, fun.args = list(mult = 1), geom = "errorbar", color = "red", width = 0.2) +
    ggplot2::stat_summary(fun = mean, geom = "point", color = "red") +
    ggplot2::ylab('DV') + ggplot2::xlab('IDV') +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) + ggplot2::theme_bw()

  L <- list(Descriptive_Statistics = Statistics, Model_summary = Model, Effect_zise = EF, Effect_interpretation = EF_exp, Post_hoc = PH, Variance_Correction = varLeven, Figure = Figure)

  freq <- table(IDV)
  if(Parametric == TRUE & sum(as.numeric(freq < 30)) != 0){
    if(varLeven == FALSE){
      lmModel <- stats::lm(formula = DV ~ IDV, data = Data)
      Res <- lmModel$residuals
    }else{
      lmModel <- estimatr::lm_robust(formula = DV ~ IDV, se_type = 'HC2', data = Data)
      Res <- Data$DV - lmModel$fitted.values
    }
    shapiroTest <- stats::shapiro.test(Res)
    if(shapiroTest$p.value < .05){
      Warning <- c("Warning: At list one of the groups has fewer than 30 observations, and the residuals' distribution is not normal. Considre to use an kruskal-wallis test (Parametric = FALSE)") %>%
        print()
    }
  }

  return(L)

}

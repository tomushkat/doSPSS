#' One Way Anova
#'
#'
#' This function conducts a tyoe 3 one way ANOVA or with hc2 correction type (if the variances are unequal). If the model is significant, an effect size and pairwise comparisons  also produced
#' Based on stats::aov(), car::Anova()
#'
#' @param DV A vector with the independent variable
#' @param IDV A vector with the dependent variable
#' @param Correction The type of correction for post hoc (default is Benjamini, Y., and Hochberg, Y. (1995)) for more details go to the function pairwise.t.test
#' @param Parametric If FALSE the test is Kruskal-Wallis test with Mann-Whitney test for post hoc pairwise comparisons
#'
#' @return A list with (1) Descriptive statistics, (2) The model, (3) Effect size - if the model is significant, (4) t-test pairwise comparisons with correction - if significant, (5) Figure
#' @export
#'
#' @examples oneWayAnova(theData$Score, theData$Condition)
#'
oneWayAnova <- function(DV, IDV, Correction = 'BH', Parametic = TRUE){

  Data <- data.frame(DV, IDV)
  Data <- Data[stats::complete.cases(Data), ]
  Data <- Data %>%
    dplyr::mutate(IDV = as.factor(IDV))

  Statistics <- Data %>%
    dplyr::group_by(IDV) %>%
    dplyr::summarise(
      Mean   = round(mean(DV), 2),
      SD     = round(stats::sd(DV), 2),
      Median = round(stats::median(DV), 2),
      N      = length(DV)
    )

  if(Parametic == TRUE){
    Leven       <- car::leveneTest(Data$DV ~ Data$IDV)
    varLeven    <- ifelse(Leven$`Pr(>F)`[1] < .05, TRUE, FALSE)
    modelOneWay <- stats::aov(DV ~ IDV, data = Data)
    Model       <- car::Anova(modelOneWay, type = 'III', white.adjust = varLeven)
    if(Model$`Pr(>F)`[2] < 0.05){
      PH <- postHoc(Data$DV, Data$IDV, Paired = FALSE)
      EF <- effectsize::effectsize(modelOneWay, type = 'eta')
    }else{
      PH <- NULL
      EF <- NULL
    }
  }else{
    Model <- stats::kruskal.test(DV ~ IDV, data = Data)
    if(Model$p.value < 0.05){
      PH <- postHoc(Data$DV, Data$IDV, Paired = FALSE, Parametic = FALSE)
      EF <- NULL
    }
  }

  Figure <- ggplot2::ggplot(Data, mapping = ggplot2::aes(x = IDV, y = DV, fill = IDV)) +
    ggplot2::geom_boxplot(color = 'purple', alpha = 2) +
    ggplot2::geom_violin(alpha = 0.1) +
    ggplot2::geom_jitter(ggplot2::aes(x = IDV, y = DV, fill = IDV)) +
    ggplot2::stat_summary(fun.data = ggplot2::mean_sdl, fun.args = list(mult = 1), geom = "errorbar", color = "red", width = 0.2) +
    ggplot2::stat_summary(fun = mean, geom = "point", color = "red") +
    ggplot2::ylab('DV') + ggplot2::xlab('IDV') +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) + ggplot2::theme_bw()

  L <- list(Descriptive_Statistics = Statistics, Model_summary = Model, Effect_zise = EF, Post_hoc = PH, Figure = Figure)

  return(L)

}

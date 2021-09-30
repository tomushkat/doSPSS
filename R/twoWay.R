#' Two Way Anova
#'
#' This function conducts a type 3 two way ANOVA or with hc2 correction type (if the variances are unequal). If the model is significant, an effect size and pairwise comparisons also produced
#' Based on stats::aov(), car::Anova()
#'
#' @param DV A vector with the dependent variable
#' @param IDV1 A vector with the first independent variable
#' @param IDV2 A vector with the second independent variable
#' @param Correction The type of correction for post hoc (default is Benjamini, Y., and Hochberg, Y. (1995)) for more details go to the function pairwise.t.test
#'
#' @return A list with the following components:
#' @return Descriptive_statistics: Descriptive statistics with the Mean, standard deviation, Median and N
#' @return Model_summary: Model's summary (with or without correction for variance)
#' @return Effect_size: eta square effect size (if the model is significant and parametric)
#' @return Post_hoc_IDV1: Pairwise comparisons for the first independent variable (if significant)
#' @return Post_hoc_IDV2: Pairwise comparisons for the second independent variable (if significant)
#' @return Post_hoc_Interaction: Pairwise comparisons for the interaction (if significant)
#' @return Figure
#' @export
#'
#' @examples twoWay(theData$Score, theData$Condition, theData$Gender)
twoWay <- function(DV, IDV1, IDV2, Correction = 'BH'){

  library(doSPSS)
  DV <- theData$Score
  IDV1 <- theData$Gender
  IDV2 <- theData$Condition

  Data <- data.frame(DV, IDV1, IDV2)
  Data <- Data[stats::complete.cases(Data), ]
  Data <- Data %>%
    dplyr::mutate(IDV1 = as.factor(IDV1),
                  IDV2 = as.factor(IDV2))

  Statistics <- Data %>%
    dplyr::group_by(IDV1, IDV2) %>%
    dplyr::summarise(
      Mean   = round(mean(DV), 2),
      SD     = round(stats::sd(DV), 2),
      Median = round(stats::median(DV), 2),
      N      = length(DV)
    )

    Leven1       <- car::leveneTest(Data$DV ~ Data$IDV1)
    Leven2       <- car::leveneTest(Data$DV ~ Data$IDV2)
    Leven3       <- car::leveneTest(Data$DV ~ Data$IDV1 * Data$IDV2)
    varLeven    <- ifelse(Leven1$`Pr(>F)`[1] < .05 | Leven2$`Pr(>F)`[1] < .05 | Leven3$`Pr(>F)`[1] < .05, TRUE, FALSE)
    modelTwoWay <- stats::aov(DV ~ IDV1 * IDV2, data = Data)
    Model       <- car::Anova(modelTwoWay, type = 'III', white.adjust = varLeven)

    if(Model$`Pr(>F)`[2] < 0.05 | Model$`Pr(>F)`[3] < 0.05 | Model$`Pr(>F)`[4] < 0.05){
      EF <- effectsize::effectsize(modelTwoWay, type = 'eta')
      if(Model$`Pr(>F)`[2] < 0.05){
        PHIDV1 <- postHoc(Data$DV, Data$IDV1, Paired = FALSE)
      }else{PHIDV1 <- NULL}
      if(Model$`Pr(>F)`[3] < 0.05){
        PHIDV2 <- postHoc(Data$DV, Data$IDV2, Paired = FALSE)
      }else{PHIDV2 <- NULL}
      if(Model$`Pr(>F)`[4] < 0.05){
        Data <- Data %>%
          dplyr::mutate(phIDV = paste0(IDV1, IDV2))
        PHinteraction <- postHoc(Data$DV, Data$phIDV, Paired = FALSE)
      }else{PHinteraction <- NULL}
    }else{
      PHIDV1 <- NULL
      PHIDV2 <- NULL
      PHinteraction <- NULL
      EF <- NULL
    }

  Figure <- ggplot2::ggplot(Data, mapping = ggplot2::aes(x = IDV1, y = DV, fill = IDV2)) +
    ggplot2::geom_boxplot(color = 'purple', alpha = 2, position = position_dodge(0.8)) +
    ggplot2::geom_violin(alpha = 0.1, position = position_dodge(0.8)) +
    ggplot2::geom_jitter(ggplot2::aes(x = IDV1, y = DV, fill = IDV2), position = position_dodge(0.8)) +
    ggplot2::stat_summary(fun.data = ggplot2::mean_sdl, fun.args = list(mult = 1), geom = "errorbar", color = "red", width = 0.2, position = position_dodge(0.8)) +
    ggplot2::stat_summary(fun = mean, geom = "point", color = "red", position = position_dodge(0.8)) +
    ggplot2::ylab('DV') + ggplot2::xlab('IDV1') +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) + ggplot2::theme_bw()

  L <- list(Descriptive_Statistics = Statistics, Model_summary = Model, Effect_zise = EF, Post_hoc_IDV1 = PHIDV1, Post_hoc_IDV2 = PHIDV2, Post_hoc_Interaction = PHinteraction, Figure = Figure)

  return(L)

}

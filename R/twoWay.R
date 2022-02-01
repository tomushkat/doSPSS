#' Two Way Anova
#'
#' This function conducts a type 3 two way ANOVA or with hc2 correction type (if the variances are unequal). If the model is significant, an effect size and pairwise comparisons also produced
#' Based on stats::aov(), car::Anova()
#'
#' @param DV A vector with the dependent variable
#' @param IDV1 A vector with the first independent variable
#' @param IDV2 A vector with the second independent variable
#' @param Correct The type of correction for post hoc (default is Benjamini, Y., and Hochberg, Y. (1995)) for more details go to the function pairwise.t.test
#'
#' @return A list with the following components:
#' @return Descriptive_statistics: Descriptive statistics with the Mean, standard deviation, Median and N
#' @return Model_summary: Model's summary (with or without correction for variance)
#' @return Effect_size: eta square effect size (if the model is significant and parametric)
#' @return Post_hoc_IDV1: Pairwise comparisons for the first independent variable (if significant)
#' @return Post_hoc_IDV2: Pairwise comparisons for the second independent variable (if significant)
#' @return Post_hoc_Interaction: Pairwise comparisons for the interaction (if significant)
#' @return Variance_Correction: If TRUE a correction for the variance was conducted (the test type is 'Analysis of Deviance')
#' @return Figure
#' @export
#'
#' @examples twoWay(DV = simulateData$Score, IDV1 = simulateData$Condition, IDV2 = simulateData$Gender)
twoWay <- function(DV, IDV1, IDV2, Correct = 'BH'){

  # Parameters for validation
  # DV = simulateData$Score
  # IDV1 = simulateData$Condition
  # IDV2 = simulateData$Gender

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

    Leven1       <- car::leveneTest(Data$DV ~ Data$IDV1)   # Variance test for IDV 1
    Leven2       <- car::leveneTest(Data$DV ~ Data$IDV2)   # Variance test for IDV 2
    Leven3       <- car::leveneTest(Data$DV ~ Data$IDV1 * Data$IDV2) # Variance test for IDV 1 and 2 interaction
    varLeven     <- ifelse(Leven1$`Pr(>F)`[1] < .05 | Leven2$`Pr(>F)`[1] < .05 | Leven3$`Pr(>F)`[1] < .05, TRUE, FALSE)  # if at list onw of the variances are not equals than TRUE for performing correction
    modelTwoWay  <- stats::aov(formula = DV ~ IDV1 * IDV2, data = Data)    # Performing the two way ANOVA with stats package
    Model        <- car::Anova(mod = modelTwoWay, type = 'III', white.adjust = varLeven)  # Performing the model with correction (optional) and as type 3 ANOVA using the car package
    ModelEF      <- car::Anova(mod = modelTwoWay, type = 'III')

    if(Model$`Pr(>F)`[2] < 0.05 | Model$`Pr(>F)`[3] < 0.05 | Model$`Pr(>F)`[4] < 0.05){  # If at list one of the effect is significant

      EF <- effectsize::eta_squared(model = ModelEF,                                    # Perform as effect size
                                    ci = .95, alternative = "two.sided")

      if(Model$`Pr(>F)`[2] < 0.05 & length(unique(Data$IDV1)) > 2){   # Test weather there are more than two levels for IDV 1 and it is significant

        phIDV1 <- postHoc(DV = Data$DV, IDV = Data$IDV1, Paired = FALSE, Correction = Correct)   # Performing a post hoc for IDV 1

      }else{phIDV1 <- NULL}

      if(Model$`Pr(>F)`[3] < 0.05 & length(unique(Data$IDV2)) > 2){   # Test weather there are more than two levels for IDV 2 and it is significant

        phIDV2 <- postHoc(DV = Data$DV, IDV = Data$IDV2, Paired = FALSE, Correction = Correct)  # Performing a post hoc for IDV 2

      }else{phIDV2 <- NULL}

      if(Model$`Pr(>F)`[4] < 0.05){   # Test weather the interaction is significant

        Data <- Data %>%
          dplyr::mutate(phIDV = paste0(IDV1, IDV2))   # Creating a new IDV variable for the interaction
        phInteraction <- postHoc(DV = Data$DV, IDV = Data$phIDV, Paired = FALSE, Correction = Correct)  # Performing a post hoc for the interaction

      }else{phInteraction <- NULL}

    }else{   # If there is no effect than every thins is NULL

      phIDV1        <- NULL
      phIDV2        <- NULL
      phInteraction <- NULL
      EF            <- NULL

    }

  Figure <- ggplot2::ggplot(Data, mapping = ggplot2::aes(x = IDV1, y = DV, fill = IDV2)) +
    ggplot2::geom_boxplot(color = 'purple', alpha = 2, position = ggplot2::position_dodge(0.8)) +
    ggplot2::geom_violin(alpha = 0.1, position = ggplot2::position_dodge(0.8)) +
    # ggplot2::geom_jitter(ggplot2::aes(x = IDV1, y = DV, fill = IDV2), position = ggplot2::position_dodge(0.8)) +
    ggplot2::stat_summary(fun.data = ggplot2::mean_sdl, fun.args = list(mult = 1), geom = "errorbar", color = "red", width = 0.2, position = ggplot2::position_dodge(0.8)) +
    ggplot2::stat_summary(fun = mean, geom = "point", color = "red", position = ggplot2::position_dodge(0.8)) +
    ggplot2::ylab('DV') + ggplot2::xlab('IDV1') +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) + ggplot2::theme_bw()

  L <- list(Descriptive_Statistics = Statistics, Model_summary = Model, Effect_zise = EF, Post_hoc_IDV1 = phIDV1, Post_hoc_IDV2 = phIDV2, Post_hoc_Interaction = phInteraction, Variance_Correction = varLeven, Figure = Figure)

  freq <- table(IDV1, IDV2)
  if(Parametric == TRUE & (sum(as.numeric(freq < 30)) > 0)){
    if(trueVarTest == FALSE){
      lmModel <- stats::lm(formula = DV ~ IDV1 * IDV2, data = Data)
      Res <- lmModel$residuals
    }else{
      lmModel <- estimatr::lm_robust(formula = DV ~ IDV1 * IDV2, se_type = 'HC2', data = Data)
      Res <- Data$DV - lmModel$fitted.values
    }
    shapiroTest <- stats::shapiro.test(Res)
    if(shapiroTest$p.value < .05){
      Warning <- c("Warning: At list one of the groups has fewer than 30 observations, and the residuals' distribution is not normal.")
      Warning
    }
  }

  return(L)

}

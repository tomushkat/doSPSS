#' Repeated measures One Way Anova
#'
#' This function conducts repeated measures ANOVA. If the model is significant, an effect size and pairwise comparisons also be produced. The data set should be with exact n observations per ID, otherwise the figure's and descriptive statistics' results will be inaccurate
#' Based on stats::aov()
#'
#' @param DV A vector with the dependent variable
#' @param IDV A vector with the independent variable
#' @param Within The identity of the cases / The identity of the within factor
#' @param Parametric If FALSE the test is Friedman test with Wilcoxon Signed Rank tests for post hoc pairwise comparisons
#' @param Correct The type of correction for post hoc (default is Benjamini, Y., and Hochberg, Y. (1995)) for more details go to the function pairwise.t.test
#'
#'
#' @return A list with the following components:
#' @return Descriptive_statistics: Descriptive statistics with the Mean, standard deviation, Median and N
#' @return Model_summary: Model's summary
#' @return Effect_size: eta square effect size (if the model is significant and parametric)
#' @return Post_hoc: Pairwise comparisons (if the model is significant)
#' @return Figure
#' @export
#'
#' @examples rmAnova(DV = simulateData$gameTime, IDV = simulateData$measureTime, Within = simulateData$ID)
#'
rmAnova <- function(DV, IDV, Within, Parametric = TRUE, Correct = 'BH'){

  Data <- data.frame(Within, DV, IDV)
  Data <- Data[stats::complete.cases(Data), ]
  Data <- Data %>%                            # Arranging the data by ID and IDV
    dplyr::mutate(Within = as.factor(Within)) %>%
    dplyr::arrange(Within, IDV)

  Statistics <- Data %>%
    dplyr::group_by(IDV) %>%
    dplyr::summarise(
      Mean   = round(mean(DV), 2),
      SD     = round(stats::sd(DV), 2),
      Median = round(stats::median(DV), 2),
      N      = length(DV)
    )

  if(Parametric == TRUE){  # if the model is  parametric

    Model <- stats::aov(formula = DV ~ IDV + Error(Within / IDV), data = Data) # Preforming the ANOVA with stats package
    sumModel <- summary(Model)

    if(sumModel$`Error: Within:IDV`[[1]][[5]][1] < 0.05){   # If the model is significant

      PH <- postHoc(DV = Data$DV, IDV = Data$IDV, Within = Data$Within, Paired = TRUE, Parametric = TRUE, Correction = Correct)  # Preform post hoc
      EF <- effectsize::effectsize(model = Model,
                                   type = 'eta', ci = .95, alternative = "two.sided")  # Perform effect size

    }else{PH <- NULL
          EF <- NULL}

  }else{

    sumModel <- stats::friedman.test(formula = DV ~ Within | IDV, data = Data)  # if not parametric, perform friedman
    EF       <- NULL

    if(sumModel$p.value < 0.05){ # If the model is significant
      PH <- postHoc(DV = Data$DV, IDV = Data$IDV, Within = Data$Within, Paired = TRUE, Parametric = FALSE, Correction = Correct) # Perform post hoc

    }else{PH <- NULL}
  }

  Figure <-
    ggplot2::ggplot(Data, ggplot2::aes(x = IDV, y = DV, fill = IDV)) +
    ggplot2::geom_boxplot(color = 'purple', alpha = 2) +
    ggplot2::geom_violin(alpha = 0.1) +
    # ggplot2::geom_jitter(ggplot2::aes(x = IDV, y = DV, fill = IDV)) +
    ggplot2::stat_summary(fun.data = ggplot2::mean_sdl, fun.args = list(mult = 1),
                          geom = "errorbar", color = "red", width = 0.2) +
    ggplot2::stat_summary(fun = mean, geom = "point", color = "red") +
    ggplot2::ylab('DV') + ggplot2::xlab('IDV') +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) + ggplot2::theme_bw()

  L <- list(Descriptive_Statistics = Statistics, Model_summary = sumModel, Effect_zise = EF, Post_hoc = PH, Figure = Figure)

  return(L)

}

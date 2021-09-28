#' Repeated measures One Way Anova
#'
#' This function conducts repeated measures ANOVA. If the model is significant, an effect size and pairwise comparisons also be produced. The data set should be with exact n observations per ID, otherwise the figure's and descriptive statistics' results will be inaccurate
#' Based on stats::aov()
#'
#' @param DV A vector with the independent variable
#' @param IDV A vector with the dependent variable
#' @param ID The identity of the observations
#' @param Correction he type of correction for post hoc (default is Benjamini, Y., and Hochberg, Y. (1995)) for more details go to the function pairwise.t.test
#'
#' @return A list with (1) Descriptive statistics, (2) The model, (3) Effect size - if the model is significant, (4) t-test pairwise comparisons with correction - if significant, (5) Figure
#' @export
#'
#' @examples rmAnova(theData$Score, theData$Condition, theData$ID)
#'
rmAnova <- function(DV, IDV, ID, Correction = 'BH'){

  Data <- data.frame(ID, DV, IDV)
  Data <- Data[stats::complete.cases(Data), ]
  Data <- Data %>%
    dplyr::mutate(ID = as.factor(ID)) %>%
    dplyr::arrange(ID, IDV)

  Statistics <- Data %>%
    dplyr::group_by(IDV) %>%
    dplyr::summarise(
      Mean   = round(mean(DV), 2),
      SD     = round(stats::sd(DV), 2),
      Median = round(stats::median(DV), 2),
      N      = length(DV)
    )

  Model <- stats::aov(DV ~ IDV + Error(ID / IDV), data = Data)
  sumModel <- summary(Model)

  if(sumModel$`Error: ID:IDV`[[1]][[5]][1] < 0.05){

    PH <- postHoc(Data$DV, Data$IDV, Data$ID, Paired = TRUE)
    EF <- effectsize::effectsize(Model, type = 'eta')

  }else{

    PH <- NULL
    EF <- NULL

  }

  Figure <-
    ggplot2::ggplot(Data, ggplot2::aes(x = IDV, y = DV, fill = IDV)) +
    ggplot2::geom_boxplot(color = 'purple', alpha = 2) +
    ggplot2::geom_violin(alpha = 0.1) +
    ggplot2::geom_jitter(ggplot2::aes(x = IDV, y = DV, fill = IDV)) +
    ggplot2::stat_summary(fun.data = ggplot2::mean_sdl, fun.args = list(mult = 1),
                          geom = "errorbar", color = "red", width = 0.2) +
    ggplot2::stat_summary(fun = mean, geom = "point", color = "red") +
    ggplot2::ylab('DV') + ggplot2::xlab('IDV') +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) + ggplot2::theme_bw()

  L <- list(Descriptive_Statistics = Statistics, Model_summary = Model, Effect_zise = EF, Post_hoc = PH, Figure = Figure)

  return(L)

}

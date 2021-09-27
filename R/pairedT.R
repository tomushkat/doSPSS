#' Paired samples t-test
#'
#' The function conducts paired sample t-test. The data set should be with exact n observations per ID, otherwise the figure's and descriptive statistics' results will be inaccurate.
#' Based on stats::t.test()
#'
#' @param ID The ID of the participants
#' @param DV A vector with the independent variable
#' @param IDV A vector with the dependent variable
#'
#' @return A list with the descriptive statistics, the model, effect size (if the model is significant and a figure)
#' @export
#'
#' @examples
#'
pairedT <- function(DV, IDV, ID){

  Data <- data.frame(DV, IDV, ID)
  Data <- Data[stats::complete.cases(Data), ]
  Data <- Data %>%
    dplyr::arrange(ID, IDV)

  Statistics <- Data %>%
    dplyr::group_by(IDV) %>%
    dplyr::summarise(
      Mean   = round(mean(DV), 2),
      SD     = round(stats::sd(DV), 2),
      Median = round(stats::median(DV), 2),
      N      = length(DV)
    )

  Model <- stats::t.test(DV ~ IDV, paired = TRUE, data = Data)

  if(Model$p.value < 0.05){
    EF <- effectsize::effectsize(Model, type = 'cohens_d')
  }else{
    EF <- NULL
  }

  Figure <-
    ggplot2::ggplot(Data, aes(x = IDV, y = DV, fill = IDV)) +
    geom_boxplot(color = 'purple', alpha = 2) +
    geom_violin(alpha = 0.1) +
    scale_fill_manual(values = c('lightgrey', 'lightgreen')) +
    geom_jitter(aes(x = IDV, y = DV, fill = IDV)) +
    stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
                 geom = "errorbar", color = "red", width = 0.2) +
    stat_summary(fun.y = mean, geom = "point", color = "red") +
    ylab('DV') + xlab('IDV') +
    theme(plot.title = element_text(hjust = 0.5)) + theme_bw()

  L <- list(Statistics, Model, EF, Figure)

  return(L)

}

#' Independent samples t-test
#'
#' The function uses the 'stats' package to conduct the independent sample t-test or the Welch test
#' The correction for the Welch test is automatically using the Leven test
#' Based on stats::t.test()
#'
#'
#' @param DV A vector with the independent variable
#' @param IDV A vector with the dependent variable
#'
#' @return A list with the descriptive statistics, the model, effect size (if the model is significant) and a figure
#' @export
#'
#' @examples
#'
#'
indttest <- function(DV, IDV){

  Data <- data.frame(DV, IDV)
  Data <- Data[stats::complete.cases(Data), ]

  Statistics <- Data %>%
    dplyr::group_by(IDV) %>%
    dplyr::summarise(
      Mean   = round(mean(DV), 2),
      SD     = round(stats::sd(DV), 2),
      Median = round(stats::median(DV), 2),
      N      = length(DV)
    )

  varTest     <- stats::var.test(DV ~ IDV, data = Data)
  trueVarTest <- ifelse(varTest$p.value > 0.05, TRUE, FALSE)
  Model       <- stats::t.test(DV ~ IDV, var.equal = trueVarTest, data = Data)

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

  L <- list(Descriptive_statistics = Statistics, Model_summary = Model, Effect_size = EF, Figure = Figure)

  return(L)

}

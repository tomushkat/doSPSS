#' Cross table / Chi-square test
#'
#' This function conducts a Chi-square test with a fisher correction (if needed).
#' Based on gmodels::CrossTable()
#'
#' @param firstFactor The variable to be locaated in the rows
#' @param colFactor The variable to be locaated in the columns
#' @param freqCorrect Minimum observations per cell for Fisher correction - default is 5
#'
#' @return Printed model with and a returned list with the following components:
#' @return Effect_size: Phi or Cramér's V effect size (if the model is significant)
#' @return Figure
#' @export
#'
#' @examples CT(rowFactor = simulateData$Gender, colFactor = simulateData$Condition)
#'
CT <- function(rowFactor, colFactor, freqCorrect = 5){

  Data <- data.frame(rowFactor, colFactor)
  Data <- Data[stats::complete.cases(Data), ]

  sumation1 <- Data %>%
    dplyr::group_by(rowFactor) %>%
    dplyr::summarise(S = length(colFactor))  # Calculation the total observations for each row

  L1 <- nrow(sumation1)  # Calculation of how many rows

  sumation2 <- Data %>%
    dplyr::group_by(colFactor) %>%
    dplyr::summarise(S = length(rowFactor))   # Calculation the total observations for each column

  L2 <- nrow(sumation2)   # Calculation of how many columns

  doFisher <- ifelse(sum(as.numeric(sumation1$S < L2 * freqCorrect)) | sum(as.numeric(sumation2$S < L1 * freqCorrect))  > 0,
                     TRUE, FALSE)   # If in one of the cells there are less observations than the limit (freqCorrect) than TRUE (a Fisher Exact Test will be conducted)
  Figure <-
    ggplot2::ggplot(Data, mapping = ggplot2::aes(x = rowFactor,  group = colFactor)) +
    ggplot2::geom_bar(ggplot2::aes(y = ..prop.., fill = factor(..x..)), stat = "count") +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(..prop..),
                                    y = ..prop..), stat = "count", vjust = -0.5) +
    ggplot2::labs(y = "Percent") +
    ggplot2::facet_grid(~colFactor) +
    ggplot2::scale_y_continuous(labels = scales::percent) + ggplot2::theme_bw() +
    ggplot2::xlab('')

  EFmodel <- stats::chisq.test(x = rowFactor, y = colFactor,
                               correct = FALSE)  # Performing a Chi square for the effect size calculation

  #  if(doFisher == TRUE){
  #   if(Model$chisq[3] < 0.05){
  #     EF <- effectsize::effectsize(EFmodel)
  # }else if(Model$fisher.ts[1] < 0.05)
  #   EF <- effectsize::effectsize(EFmodel)
  # }else{
  #   EF <- NULL

  if(EFmodel$p.value < 0.05){   # If the model is significant

    if(L1 == 2 & L2 == 2){  # If there are 4 cells in total Phi value will be produced

      EF <- effectsize::effectsize(model = EFmodel,
                                   type = 'phi', ci = .95, alternative = "two.sided")

    }else{EF <- effectsize::effectsize(model = EFmodel,
                                   type = 'cramers_v', ci = .95, alternative = "two.sided")}  # If there are more than 4 cells in total, Cramer's V will be produced

  }else{

    EF <- NULL

  }

  L <- list(Effect_size = EF, Figure = Figure)

  gmodels::CrossTable(rowFactor, colFactor, chisq = TRUE, format = 'SPSS', fisher = doFisher)  # Conducting and printing the model (with Fisher Exact Test if needed) and a cross table

  return(L)

}

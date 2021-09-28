#' Cross table / Chi-square test
#'
#' This function conducts a Chi-square test with a fisher correction (if needed).
#' Based on gmodels::CrossTable()
#'
#' @param DV The dependent variable
#' @param IDV The independent variable
#' @param freqCorrect Minimum observations per cell for Fisher correction - default is 5
#'
#' @return Prints the Model, and returns the effect size (if the model is significant) and a figure
#' @export
#'
#' @examples CT(theData$Gender, theData$Condition)
#'
CT <- function(DV, IDV, freqCorrect = 5){


  Data <- data.frame(DV, IDV)
  Data <- Data[stats::complete.cases(Data), ]

  sumation1 <- Data %>%
    dplyr::group_by(DV) %>%
    dplyr::summarise(S = length(IDV))

  L1 <- nrow(sumation1)

  sumation2 <- Data %>%
    dplyr::group_by(IDV) %>%
    dplyr::summarise(S = length(DV))

  L2 <- nrow(sumation2)

  doFisher <- ifelse(sum(as.numeric(sumation1$S < L1 * freqCorrect)) | sum(as.numeric(sumation2$S < L2 * freqCorrect))  > 0,
                     TRUE, FALSE)
  Figure <-
    ggplot2::ggplot(Data, mapping = ggplot2::aes(x = DV,  group = IDV)) +
    ggplot2::geom_bar(ggplot2::aes(y = ..prop.., fill = factor(..x..)), stat = "count") +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(..prop..),
                                    y = ..prop..), stat = "count", vjust = -0.5) +
    ggplot2::labs(y = "Percent") +
    ggplot2::facet_grid(~IDV) +
    ggplot2::scale_y_continuous(labels = scales::percent) + ggplot2::theme_bw() +
    ggplot2::xlab('')

  EFmodel <- stats::chisq.test(DV, IDV, correct = FALSE)


  #  if(doFisher == TRUE){
  #   if(Model$chisq[3] < 0.05){
  #     EF <- effectsize::effectsize(EFmodel)
  # }else if(Model$fisher.ts[1] < 0.05)
  #   EF <- effectsize::effectsize(EFmodel)
  # }else{
  #   EF <- NULL

  if(EFmodel$p.value < 0.05){
    EF <- effectsize::effectsize(EFmodel)
  }else{
    EF <- NULL
  }

  gmodels::CrossTable(DV, IDV, chisq = TRUE, format = 'SPSS', fisher = doFisher)

  L <- list(Effect_size = EF, Figure = Figure)

  return(L)

}


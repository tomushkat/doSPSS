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
#' @examples
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
    ggplot2::ggplot(Data, aes(x = DV,  group = IDV)) +
    geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count") +
    geom_text(aes(label = scales::percent(..prop..),
                  y = ..prop..), stat = "count", vjust = -0.5) +
    labs(y = "Percent") +
    facet_grid(~IDV) +
    scale_y_continuous(labels = scales::percent) + theme_bw() +
    xlab('')

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

  L <- list(EF, Figure)

  return(L)

}


#' Cross table / Chi-square for Independence test
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

  # Parameters for validation
  # rowFactor = simulateData$Gender
  # colFactor = simulateData$Condition
  # freqCorrect = 5

  Data <- data.frame(rowFactor, colFactor) %>%
    tidyr::drop_na()

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
    ggplot2::ggplot(Data, mapping = ggplot2::aes(x = rowFactor, group = colFactor)) +
    ggplot2::geom_bar(ggplot2::aes(y = ..prop.., fill = factor(..x..)), stat = "count") +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(..prop..),
                                    y = ..prop..), stat = "count", vjust = -0.5) +
    ggplot2::labs(y = "Percent") +
    ggplot2::facet_grid(~colFactor) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::theme_bw() +
    ggplot2::xlab('')

  EFmodel <- stats::chisq.test(x = Data$rowFactor, y = Data$colFactor,
                               correct = FALSE)  # Performing a Chi square for the effect size calculation

  EF <- NULL
  EF_exp <- NULL
  if(EFmodel$p.value < 0.05){   # If the model is significant
    # if(L1 == 2 & L2 == 2){  # If there are 4 cells in total Phi value will be produced
    #     typeEF <- c('cohens_w') # phi
    # }else{
    #     typeEF <- c('cohens_w') # cramers_v
    #  }
    X <- table(Data$rowFactor, Data$colFactor)
    EF <- effectsize::cramers_v(x = X,
                                   ci = .95, alternative = "two.sided")
    EF_value <- ifelse(abs(EF$Cramers_v_adjusted) >= 0.04 & abs(EF$Cramers_v_adjusted) < 0.3, 'small effect size.',
                    ifelse(abs(EF$Cramers_v_adjusted) >= 0.3 & abs(EF$Cramers_v_adjusted) < 0.5, 'medium effect size.',
                      ifelse(abs(EF$Cramers_v_adjusted) >= 0.5, 'large effect size.', 'less than a small effect size.')))
    EF_exp <- paste0("The Cramer's v value is ", round(EF$Cramers_v_adjusted, 2), ', which is interpreted as a ', EF_value)
  }

  L <- list(Effect_size = EF, Effect_interpretation = EF_exp, Figure = Figure)

  gmodels::CrossTable(rowFactor, colFactor, chisq = TRUE, format = 'SPSS', fisher = doFisher)  # Conducting and printing the model (with Fisher Exact Test if needed) and a cross table

  return(L)

}

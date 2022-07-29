#' Chi-square test for Goodness of Fit
#'
#' This function conducts a Chi-square test with a fisher correction (if needed).
#' Based on gmodels::CrossTable()
#'
#' @param DV The variable to be tested
#' @param Probs Vector of probabilities
#'
#' @return Printed model with and a returned list with the following components:
#' @return Effect_size: Cohen's W effect size (if the model is significant)
#' @return Figure
#' @export
#'
#' @examples CT(rowFactor = simulateData$Gender, colFactor = simulateData$Condition)
#'

gF <- function (DV, Probs) {


  gmodles::CrossTable(DV, format = 'SPSS')
  Model <- stats::chisq.test(base::table(DV), p = Probs, correct = FALSE)

  EF <- "No effect size for insignificant results"

  if(Model$p.value < .05) {

    EF <- effectsize::effectsize(Model, 'cohens_w')

  }

  Figure <-
    DV %>%
    table() %>%
    as.data.frame() %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x=., y = Freq)) +
    ggplot2::geom_bar(stat="identity", position = "dodge") +
    ggplot2::geom_text(aes(label = Freq), vjust = -0.2, size = 5,
              position = position_dodge(0.9)) +
    ggplot2::ylab('Frequency')  +
    ggplot2::theme_bw()

  return(list(Model, EF, Figure))


}


#' Chi-square for Goodness of Fit test
#'
#' This function conducts a Chi-square test
#' Based on gmodels::CrossTable()
#'
#' @param DV The vDependent variable
#' @param Probs The probabilities
#'
#' @return Printed model with and a returned list with the following components:
#' @return Effect_size: Cohen's W effect size (if the model is significant)
#' @return Figure
#' @export
#'
#' @examples
GoF <- function(DV, Probs) {


  EF <- NULL
  gmodels::CrossTable(DV, format = 'SPSS')
  Model <- stats::chisq.test(base::table(DV), p = Probs, correct = FALSE)
  if(Model$p.value < .05){

    EF <- effectsize::effectsize(Model, 'cohens_w')

  }

Figure <-
  DV %>%
    base::table() %>%
    as.data.frame() %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = ., y = Freq)) +
    ggplot2::geom_bar(stat="identity", position = "dodge") +
    ggplot2::geom_text(aes(label = Freq), vjust = -0.2, size = 5,
              position = ggplot2::position_dodge(0.9)) +
    ggplot2::ylab('Frequency')  +
    ggplot2::theme_bw()



  return(list(Model, EF, Figure))


}


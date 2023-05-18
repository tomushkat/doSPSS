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
  EF_exp <- NULL
  gmodels::CrossTable(DV, format = 'SPSS')
  Model <- stats::chisq.test(base::table(DV), p = Probs, correct = FALSE)
  if(Model$p.value < .05){

    EF <- effectsize::cohens_w(x = base::table(DV), p = Probs,
                               ci = .95, alternative = "two.sided")

    EF_value <- dplyr::case_when(

      abs(EF$Cohens_w) < 0.1 ~ 'less than a small effect size.'
      , abs(EF$Cohens_w) < 0.3 ~ 'small effect size.'
      , abs(EF$Cohens_w) < 0.5 ~ 'medium effect size.'
      , T ~ 'large effect size.'

    )

    EF_exp <- paste0("The Cohen's W value is ,", round(EF$Cohens_w, 2), ', which is interpreted as a ', EF_value)

  }

Figure <-
  DV %>%
    base::table() %>%
    as.data.frame() %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = ., y = Freq)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::geom_text(mapping = ggplot2::aes(label = Freq), vjust = -0.2, size = 5,
              position = ggplot2::position_dodge(0.9)) +
    ggplot2::ylab('Frequency')  +
    ggplot2::theme_bw()



  return(list(Model_summary = Model, Effect_size = EF, Effect_interpretation = EF_exp, Figure = Figure))


}


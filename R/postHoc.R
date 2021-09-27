#' Post-hoc pairwise t-test comparisons
#'
#' This function conduct pairwise t-test comparisons.
#' Based on stats::pairwise.t.test()
#'
#' @param DV The dependent variable
#' @param IDV The independent variable
#' @param Correction The type of correction (default is Benjamini, Y., and Hochberg, Y. (1995)) for more details go to the function pairwise.t.test
#' @param Paired Should be TRUE if the test is for paired comparisons (default is FALSE)
#' @param ID The identity of the observations (mandatory for paired tests, the default is NULL)
#'
#' @return The model
#' @export
#'
#'
#' @examples
#'
postHoc <- function(DV, IDV, ID = NULL, Correction = 'BH', Paired = FALSE){

  if(Paired == FALSE){
    Leven <- car::leveneTest(DV ~ IDV)
    varLeven <- ifelse(Leven$`Pr(>F)`[1] < .05, TRUE, FALSE)
    Model <- stats::pairwise.t.test(x = DV, g = IDV,
                                    p.adjust.method = Correction,
                                    pool.sd = varLeven, paired = Paired)
  }else{
    Data <- data.frame(DV, IDV, ID)
    Data <- Data %>%
      dplyr::arrange(ID, IDV)

    Model <- stats::pairwise.t.test(x = DV, g = IDV,
                                    p.adjust.method = Correction,
                                    pool.sd = FALSE, paired = Paired)
  }

  return(Model)

}

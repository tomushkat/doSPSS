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
#' @param Parametric If TRUE the tests are t.tests, if FALSE the tests are Mann-Whitney/Wilcoxon Signed Rank tests
#'
#' @return The model summary
#' @export
#'
#'
#' @examples postHoc(DV = simulateData$gameTime, IDV = simulateData$measureTime)
#'
postHoc <- function(DV, IDV, ID = NULL, Correction = 'BH', Paired = FALSE, Parametric = TRUE){


  if(Parametric == TRUE){

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

  }else{

    if(Paired == FALSE){
      Model <- stats::pairwise.wilcox.test(x = DV, g = IDV,
                                      p.adjust.method = Correction,
                                      correct = FALSE,
                                      paired = Paired)

    }else{

      Data <- data.frame(DV, IDV, ID)
      Data <- Data %>%
        dplyr::arrange(ID, IDV)
      Model <- stats::pairwise.wilcox.test(x = DV, g = IDV,
                                      p.adjust.method = Correction,
                                      correct = FALSE,
                                      paired = Paired)

    }
  }

  return(Model)

}

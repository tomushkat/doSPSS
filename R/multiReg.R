#' Multiple or single linear regression
#'
#' The function conduct multiple linear regression.
#' The type of correction for heteroscedasticity (no correction v.s HC2 or other is done automatically)
#' If multicolliniarity is exists, the model will run, but the VIF values will be printed with a recommendation to remove variables.
#' Based on stats::lm(), estimatr::lm_robust(), faraway::vif(), lmtest::bptest()
#'
#' @param DV The dependent variables
#' @param Predictors The independent variables (order for string factors will be by the ABC)
#' @param Correction The type of correction for heteroscedasticity (default is HC2, for more information see estimatr::lm_robust se_type)
#'
#' @return The model with unstandardized beta coefficients, standardized beta coefficients, VIF and type of correction
#' @export
#'
#' @examples
#'
multiReg <- function(DV, Predictors, Correction = 'HC2'){

  Data <- data.frame(DV = DV, Predictors)

  Model <- stats::lm(DV ~ ., data = Data)

  if(ncol(Data) >= 3){
    vifValues <- faraway::vif(Model)
    Counter <- 0
    for (i in vifValues){
      if(i >= 10){
        Counter <- Counter + 1
      }
    }
    if(Counter > 0){
      print("There is a multicolliniarity in the model. One of the predictors' VIF is greater than 10. Consider to exlude predictors")
      print('The VIF values are:')
      print(vifValues)
    }
  }else{
    vifValues <- NULL
  }

  varTest <- lmtest::bptest(Model)
  regType <- ifelse(varTest$p.value < 0.05, Correction, 'classical')

  Model <- estimatr::lm_robust(DV ~ ., se_type = regType, data = Data)

  DV <- scale(DV)
  for (i in 1:ncol(Predictors)){
    if(typeof(Predictors[, i]) == 'double' | typeof(Predictors[, i]) == 'numeric' | typeof(Predictors[, i]) == 'integer'){
      Predictors[, i] <- scale(Predictors[, i])
    }
  }

  Data <- data.frame(DV = DV, Predictors)

  scaledModel <- estimatr::lm_robust(DV ~ ., se_type = regType, data = Data)
  betaCoeff   <- round(scaledModel$coefficients, 2)

  L <- list(Model_Summary = Model, Standardized_beta_Coeff = betaCoeff, VIF_Values = vifValues, se_type = regType)

  return(L)

}

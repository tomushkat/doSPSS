#' Multiple or single linear regression
#'
#' The function conduct multiple linear regression.
#' The type of correction for heteroscedasticity (no correction v.s HC2 or other is done automatically)
#' If multicolliniarity is exists, the model will run, but the VIF values will be printed with a recommendation to remove variables.
#' Based on stats::lm(), estimatr::lm_robust(), faraway::vif(), lmtest::bptest()
#'
#' @param DV The dependent variables
#' @param Predictors The independent variables (order for string factors will be by the ABC)
#' @param Correct The type of correction for heteroscedasticity (default is HC2, for more information see estimatr::lm_robust se_type)
#'
#' @return A list with the following components:
#' @return Model_Summary: The model's summary with unstandardized beta coefficients,
#' @return Standardized_beta_Coeff: Standardized beta coefficients
#' @return VIF_Values: VIF values
#' @return se_type: Type of correction for heteroscedasticity if was used
#' @export
#'
#' @examples
#'
multiReg <- function(DV, Predictors, Correct = 'HC2'){

  Data <- data.frame(DV = DV, Predictors)

  Model <- stats::lm(formula = DV ~ ., data = Data)

  if(ncol(Data) >= 3){

    vifValues <- faraway::vif(object = Model)
    Counter   <- 1
    nValues   <- ncol(Data) - 1
    Continue  <- 1

    while(Continue == 1 & Counter <= nValues){

      if(vifValues[Counter] >= 10){

        Continue <- 0

      }else{Counter <- Counter + 1}

    }

  }else{vifValues <- NULL}

  varTest <- lmtest::bptest(formula = Model)
  regType <- ifelse(varTest$p.value < 0.05, Correct, 'classical')

  Model <- estimatr::lm_robust(formula = DV ~ ., se_type = regType, data = Data)

  DV <- scale(DV)
  for (columnIndex in 1:ncol(Predictors)){

    if(typeof(Predictors[, columnIndex]) == 'double' | typeof(Predictors[, columnIndex]) == 'numeric' | typeof(Predictors[, columnIndex]) == 'integer'){

      Predictors[, columnIndex] <- scale(Predictors[, columnIndex])

    }
  }

  Data <- data.frame(DV = DV, Predictors)

  scaledModel <- estimatr::lm_robust(formula = DV ~ ., se_type = regType, data = Data)
  betaCoeff   <- round(scaledModel$coefficients, 2)

  L <- list(Model_Summary = Model, Standardized_beta_Coeff = betaCoeff, VIF_Values = vifValues, se_type = regType)

  return(L)

  if(Continue == 0){

    print("Warning: There is a multicolliniarity in the model. One of the predictors' VIF is greater than 10. Consider to exlude predictors")
    print('The VIF values are:')
    print(vifValues)

  }

}

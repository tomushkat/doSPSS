#' Multiple or single linear regression
#'
#' The function conduct multiple linear regression.
#' The type of correction for heteroscedasticity (no correction v.s HC2 or other is done automatically)
#' If multicollinearity is exists, the model will run, but the VIF values will be printed with a recommendation to remove variables.
#' Based on stats::lm(), estimatr::lm_robust(), faraway::vif(), lmtest::bptest()
#'
#' @param DV The dependent variables
#' @param Predictors The independent variables (order for string factors will be by the ABC)
#' @param Correct The type of correction for heteroscedasticity (default is HC2, for more information see estimatr::lm_robust se_type)
#'
#' @return A list with the following components:
#' @return Model_Summary: The model's summary with unstandardized beta coefficients,
#' @return Standardized_beta_Coeff: Standardized beta coefficients
#' @return Model_Statistics: The model's F value as a sentance
#' @return VIF_Values: VIF values
#' @return se_type: Type of correction for heteroscedasticity if was used
#' @export
#'
#' @examples multiReg(DV = simulateData$Score, Predictors = simulateData[, c('Age', 'Condition', 'gameTime')])
#'
multiReg <- function(DV, Predictors, Correct = 'HC2'){

  #Parameters for test
  # DV = simulateData$Score
  # Predictors = simulateData[, c('Age', 'Condition', 'gameTime')]


  Data <- data.frame(DV = DV, Predictors)

  Model <- stats::lm(formula = DV ~ ., data = Data)  # Predicting a linear model without correction

  vifValues <- NULL
  if(ncol(Data) >= 3){                         # If there are more than 1 predictor that calculating the VIF values. If one of the values is greater than 10 a warning will be printed.

    vifValues <- faraway::vif(object = Model)
    Counter   <- 1
    nValues   <- ncol(Data) - 1
    Continue  <- 1

    while(Continue == 1 & Counter <= nValues){

      if(vifValues[Counter] >= 10){

        Continue <- 0

      }else{Counter <- Counter + 1}
    }
  }

  varTest <- lmtest::bptest(formula = Model)    # Testing for heteroscedasticity
  regType <- ifelse(varTest$p.value < 0.05, Correct, 'classical')  # If there is a heteroscedasticity than the type of correction will be assigned

  Model_1 <- estimatr::lm_robust(formula = DV ~ ., se_type = regType, data = Data)   # Performing the model againg with correction (if neaded)

  if(regType == 'classical'){
    betaCoeff <- effectsize::standardize_parameters(Model, robust = FALSE, method = 'refit')
  }else{
    betaCoeff <- effectsize::standardize_parameters(Model, robust = TRUE, method = 'refit')
  }

  Fv   <- Model_1$fstatistic[1]
  df1  <- Model_1$fstatistic[2]
  df2  <- Model_1$fstatistic[3]
  Radj <- round(100 * Model_1$adj.r.squared, 2)
  p    <- stats::pf(Fv, df1, df1, lower.tail = FALSE)

  if(p < 0.05){
    summaySentence <- paste0('The model was significant (F(', df1, ', ', df2, ') = ',
                             Fv, ', p = ', p, '), explaning ',  Radj, '% of the variance in the depandant variable')
  }else{

    summaySentence <- paste0('The model was not significant (F(', df1, ', ', df2, ') = ',
                             Fv, ', p = ', p, '), explaning ',  Radj, '% of the variance in the depandant variable')
  }

  L <- list(Model_Summary = Model_1, Standardized_beta_Coeff = betaCoeff, Model_Statistics = summaySentence, VIF_Values = vifValues, se_type = regType)

  if(nrow(Data < 30)){
    Res <- Model$residuals
    shapiroTest <- stats::shapiro.test(Res)
    if(shapiroTest$p.value < .05){
      Warning <- c("Warning: The residuals' distribution is not normal.")
      Warning
    }
  }

  if(Continue == 0){    # If at list one of the VIF values is greater than 10 than printing the warning together with the VIF values

    print("Warning: There is a multicollinearity in the model. One of the predictors' VIF is greater than 10. Consider to exlude predictors.")
    print('The VIF values are:')
    print(vifValues)
  }

  return(L)

}

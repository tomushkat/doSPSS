#' Logistic regression
#'
#' The function conducts logistic regression together with the preliminary tests: NagelKerke and Hosmer-Lemeshow Goodness of Fit tests.
#' Based on stats::glm(), rcompanion::nagelkerke(), ResourceSelection::hoslem.test
#'
#' @param DV The dependent variable - a numeric 0-1 vector. IF the DV is a character type, the function will give the values o and 1 using alphabetic order.
#' @param Predictors The independent variables
#' @param Classification The classification boundary for 1 values (default is 0.5)
#'
#' @return A list with the following components:
#' @return Model_Summary: The model's summary
#' @return Odds_Ratio: Odds ratios and 95% confidence intervals
#' @export
#'
#' @examples
#'
logReg <- function(DV, Predictors, Classification = 0.5){

  if(typeof(DV) == "character"){
    DV <- as.numeric(as.factor(DV))
    DV <- DV - 1
  }

  Data <- data.frame(DV = DV, Predictors)

  regLog1 <- stats::glm(DV ~ ., family = binomial('logit'), data = Data)
  ORCI    <- round(exp(cbind(Odds_Ratios = stats::coef(regLog1), stats::confint(regLog1))), 2)

  cdiff           <- round(regLog1$null.deviance - regLog1$deviance, 2)
  dfdiff          <- regLog1$df.null - regLog1$df.residual
  p               <- round(stats::pchisq(cdiff, dfdiff, lower.tail = FALSE), 100)
  Nagelkerke      <- rcompanion::nagelkerke(regLog1, null = NULL, restrictNobs = FALSE)
  NagelkerkePrint <- paste0(100 * round(Nagelkerke$Pseudo.R.squared.for.model.vs.null[3], 4), "%")

  Hoslem          <- ResourceSelection::hoslem.test(regLog1$y, fitted(regLog1), g = 10)
  binaryCorrect   <- ifelse(regLog1$fitted.values > Classification, 1, 0)
  Prediction      <- table(DV, binaryCorrect)
  Accuracy        <- paste0(round((Prediction[1, 1] + Prediction[2, 2]) / sum(Prediction) * 100, 2), "%")
  Sensitivity     <- paste0(round(Prediction[2, 2] / (Prediction[2, 2] + Prediction[1, 1]) * 100, 2), "%")
  Specificity     <- paste0(round(Prediction[1, 1] / (Prediction[1, 1] + Prediction[1, 2]) * 100, 2), "%")

  L <- list(Model_Summary = summary(regLog1), Odds_Ratio = ORCI)

  print(paste0("The model's significance by the Nagelkerke is (X2(", dfdiff, ') = ', round(cdiff, 2), ', p = ', round(p, 2), '), while explaining ', NagelkerkePrint, ' of the total variance in the dependent variable. The model fit to the data by the Hosmer-Lemeshow Goodness of Fit test is (X2(8) = ',  round(Hoslem$statistic, 2), ', p = ', round(Hoslem$p.value, 2),
         '), while classifying about ', Accuracy, ", of total observations. The model sensitivity and specificity are ",  Sensitivity, ' and ', Specificity, ' respectively.'))

  return(L)
  }

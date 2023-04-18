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
#' @return Paragraph_Summary: Paragraph which summaries the model's significance, goodness of fit and accuracy, sensitivity and specificity
#' @export
#'
#' @examples logReg(DV = simulateData$Gender, Predictors = simulateData[, c('Age', 'Score')])
#'

logReg <- function(DV, Predictors, Classification = 0.5){

  # Parameters for test
  # DV = simulateData$Gender
  # Predictors = simulateData[, c('Age', 'Score')]
  # Classification = 0.5

  if(typeof(DV) == "character"){      # If the DV is of string type, convert it to 1 and 0 (0 is the first by Alphbetic order)
    DV <- as.numeric(as.factor(DV))
    DV <- DV - 1
  }

  Data <- cbind(DV = DV, Predictors) %>%
    as.data.frame() %>%
    tidyr::drop_na()
  # Data <- Data[stats::complete.cases(Data), ]

  regLog1 <- stats::glm(DV ~ ., family = binomial('logit'), data = Data)   # Performing a logistic regression
  ORCI    <- round(exp(cbind(Odds_Ratios = stats::coef(regLog1), stats::confint(regLog1))), 2)  # Calculation odds ratios and confidance intervales for the odds ratios

  cdiff           <- round(regLog1$null.deviance - regLog1$deviance, 2)  # Calculating the Chi square value of the model
  dfdiff          <- regLog1$df.null - regLog1$df.residual              # Calculating the degrees of freedom value of the model
  p               <- round(stats::pchisq(q = cdiff, df = dfdiff, lower.tail = FALSE), 100) # Calculating the p value of the model
  Nagelkerke      <- rcompanion::nagelkerke(fit = regLog1, null = NULL, restrictNobs = FALSE)  # Calculating the explained variance
  NagelkerkePrint <- paste0(100 * round(Nagelkerke$Pseudo.R.squared.for.model.vs.null[3], 4), "%") # Extracting the explained variance

  Hoslem          <- ResourceSelection::hoslem.test(x = regLog1$y, y = fitted(regLog1), g = 10)  # Calculating the significance of the accuracy
  hoslemStatistic <- round(Hoslem$statistic, 2)
  hoslemP         <- round(Hoslem$p.value, 2)
  binaryCorrect   <- ifelse(regLog1$fitted.values > Classification, 1, 0)                        # Creating a vector of 1 and 0 (by the probabilities of each ID to be 1, and the Classification value)
  Prediction      <- table(Data$DV, binaryCorrect)                                                    # Creating a table with the predicted values and the actual DV values
  Accuracy        <- paste0(round((Prediction[1, 1] + Prediction[2, 2]) / sum(Prediction) * 100, 2), "%")   # Calculating the Accuracy
  Sensitivity     <- paste0(round(Prediction[2, 2] / (Prediction[2, 2] + Prediction[1, 1]) * 100, 2), "%")  # Calculating the sensitivity
  Specificity     <- paste0(round(Prediction[1, 1] / (Prediction[1, 1] + Prediction[1, 2]) * 100, 2), "%")  # Calculating the specificity


  paragraphSummary <- paste0("The model's significance by the Nagelkerke is (X**2(", dfdiff, ') = ', round(cdiff, 2),
                             ', p = ', round(p, 2), '), while explaining ', NagelkerkePrint,
                             ' of the total variance in the dependent variable. The model fit to the data by the Hosmer-Lemeshow Goodness of Fit test is (X**2(8) = ',
                             hoslemStatistic, ', p = ', hoslemP, '), while classifying about ', Accuracy,
                             ", of total observations. The model sensitivity and specificity are ",
                             Sensitivity, ' and ', Specificity, ' respectively.')

  L <- list(Paragraph_Summary = paragraphSummary, Model_Summary = summary(regLog1), Odds_Ratio = ORCI)

  return(L)

  }

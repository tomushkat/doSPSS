#' Calculating Mean CI using Student t distribution
#'
#' @param data input numeric vector
#' @param conidance_interval default is 95%
#'
#' @return Return a list with the mean and high and low CI
#' @export
#'
#' @examples mean_CI(simulateData$gameTime)
mean_CI <- function(data, conidance_interval = 0.95){

  alpha_threshold <- 1 - conidance_interval

  sample_mean <- mean(data, na.rm = T)

  sample_n <- length(na.omit(data))
  sample_sd <- stats::sd(data, na.rm = T)
  sample_se <- sample_sd / sqrt(sample_n)

  alpha <- alpha_threshold
  degrees_freedom <- sample_n - 1
  t_score <- stats::qt(p = alpha / 2
                , df = degrees_freedom
                , lower.tail = FALSE)

  margin_error <- t_score * sample_se
  lower_bound <- sample_mean - margin_error
  upper_bound <- sample_mean + margin_error

  return(list(M = sample_mean %>% round(2), L = lower_bound %>% round(2), H = upper_bound %>% round(2)))

}

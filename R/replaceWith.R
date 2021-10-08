#' Replace with
#'
#' This function takes a vector and if the missing values ratio are greater than a given value (0.8 default), its replaced with either the Mean (default), Median or mode
#'
#' @param data A vector
#' @param type Type of statistic (default in the mean)
#' @param Ratio Minimum ratio for replacement (default in 80%)
#'
#'
#' @return New vector with no missing values
#' @export
#'
#' @examples replaceWith(theData$Age)
#'
replaceWith <- function(data, type = 'Mean', Ratio = 0.8){

  sumNA <- sum(as.numeric(is.na(data)))
  theRatio <- sumNA / length(data)

  if(theRatio < Ratio){

    if(type == 'Mean'){
      Statistic <- mean(data, na.rm = T)

    }else if(type == 'Median'){
      Statistic <- stats::median(data, na.rm = T)

    }else if(type == 'Mode'){
      ux <- unique(stats::na.omit(data))
      Statistic <- ux[which.max(tabulate(match(stats::na.omit(data), ux)))]
    }

    print(paste0('The missing values ratio was ', round(100 * theRatio, 2), '%. The Values were replaced by the ', type, ' value = ', round(Statistic, 2)))

    data <- ifelse(is.na(data), Statistic, data)

  }else{
    print(paste0('The missing values ratio was ', round(100 * theRatio, 2), '%. The Values were not replaced.'))
  }

  return(data)
}

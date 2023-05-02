#' 'do proportion CI
#'
#' @param n
#' @param p
#'
#' @return
#' @export
#'
#' @examples
porportion_CI <- function(n, p){

  SE <- sqrt(p * (1 - p) / n)
  z_star <- qnorm(1 - (1 - 0.95) / 2)
  ME <- z_star * SE * 100

  p <- p * 100
  H <- p + ME
  L <- p - ME

  return(list(p = p %>% rounc(2), L = L %>% round(2), H = H %>% round(2)))

}

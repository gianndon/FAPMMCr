#' Zeta Function
#'
#' Returns the value of zeta. Zeta is the probability that all servers are busy when you arrive / probability that you have to wait whenn you arrive
#' @param c Number of server
#' @param utilization Utilization of the system
#' @return Probability
#' @examples zeta(c=4, utilization=0.8)
#' @examples zeta(4, 0.8)
#' @export

zeta <- function(c, utilization){
  n <- c-1
  x <- utilization*c
  psi <- (x^n/factorial(n)) / (sum(x^(0:n) / factorial(0:n)))
  z <- (utilization*psi) / (1-utilization+utilization*psi)
  return(z)
}

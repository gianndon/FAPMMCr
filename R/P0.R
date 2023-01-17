#' P0 Function
#'
#' Returns the probability that there are currently zero objects in the system
#' @param c number of servers
#' @param u utilization
#'
#' @return Probability
#' @export
#'
#' @examples P0(c=4, u=0.75)
P0 <- function(c, u){

  # calculation of zeta
  n <- c-1
  x <- u*c
  psi <- (x^n/factorial(n)) / (sum(x^(0:n) / factorial(0:n)))
  z <- (u*psi) / (1-u+u*psi)

  # calculation of p0
  return(z*factorial(c)*(1-u)/(c*u)^c)
}

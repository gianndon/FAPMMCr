#' ew_n Function
#'
#' Expected value (average) of objects in the system
#' @param c number of servers
#' @param utilization utilization of the system
#' @return numeric vector, Returns the expected value of the objects/customers in the SYSTEM.
#' @examples ew_n(c=3, utilization=0.9)
#' @examples ew_n(3, 0.9)
#' @export
#'
ew_n <- function(c, utilization){

  # calculation of zeta
  n <- c-1
  x <- utilization*c
  psi <- (x^n/factorial(n)) / (sum(x^(0:n) / factorial(0:n)))
  z <- (utilization*psi) / (1-utilization+utilization*psi)

  # calculation of expected value of objects in the system
  return((utilization*z) / (1-utilization) + c*utilization)
}

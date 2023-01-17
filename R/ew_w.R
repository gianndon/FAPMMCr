#' ew_w Function
#'
#' Average time an object spent in the system
#' @param c number of servers
#' @param utilization utilization of the system
#' @param mu processing/service rate
#' @return numeric vector, Returns the expected time an objects/customers spent in the SYSTEM.
#' @examples ew_w(c=3, utilization=0.9, mu=3)
#' @examples ew_w(3, 0.9, 3)
#' @export
#'
ew_w <- function(c, utilization, mu){

  # calculation of zeta
  n <- c-1
  x <- utilization*c
  psi <- (x^n/factorial(n)) / (sum(x^(0:n) / factorial(0:n)))
  z <- (utilization*psi) / (1-utilization+utilization*psi)

  # calculation of expected value of time an object spends in the system
  return(z / (c*mu*(1-utilization)) + 1/mu)
}

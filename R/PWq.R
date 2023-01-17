#' PWq Function
#'
#' @param c number of servers
#' @param lambda arrival rate
#' @param mu service rate
#' @param x time
#'
#' @return Probability
#' @export
#'
#' @examples PWq(c=3, lambda=4, mu=5, x=1)
PWq <- function(c, lambda, mu, x){

  # calculation of u
  utilization <- lambda/(c*mu)

  # calculation of zeta
  n <- c-1
  xz <- utilization*c
  psi <- (xz^n/factorial(n)) / (sum(xz^(0:n) / factorial(0:n)))
  z <- (utilization*psi) / (1-utilization+utilization*psi)

  # calculation of ew_q
  return(z*exp(-c*mu*(1-utilization)*x))
}

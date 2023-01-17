#' PW Function
#'
#' Returns the probability that the cycle time is larger than x
#' @param c number of servers
#' @param lambda arrival rate
#' @param mu service rate
#' @param x time
#'
#' @return Probability
#' @export
#'
#' @examples PW(c=3, lambda=4, mu=5, x=2)
PW <- function(c, lambda, mu, x){

  # calculation of utilization
  u <- lambda/(c*mu)

  # calculation of zeta
  n <- c-1
  xz <- u*c
  psi <- (xz^n/factorial(n)) / (sum(xz^(0:n) / factorial(0:n)))
  z <- (u*psi) / (1-u+u*psi)

  # Fallunterscheidungen
  if((lambda/mu) == (c-1)){
    return((1+z*mu*x)*exp(-mu*x))
  }
  else if((lambda/mu) != (c-1)){
    term1 <- exp(-mu*x)
    term2 <- 1+z/(c-1-c*u)
    term3 <- 1-exp(-mu*x*(c-1-c*u))
    return(term1*term2*term3)
  }
}

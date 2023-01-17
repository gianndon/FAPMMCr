#' Pn Function
#'
#' Returns the probability of having n items in the system
#' @param c number of servers
#' @param u utilization
#' @param n number of items
#'
#' @return Probability
#' @export
#'
#' @examples Pn(c=5, u=0.85, n=2)
Pn <- function(c, u, n){

  # calculation of zeta
  N <- c-1
  x <- u*c
  psi <- (x^N/factorial(N)) / (sum(x^(0:N) / factorial(0:N)))
  z <- (u*psi) / (1-u+u*psi)

  # calculation of p0
  p0 <- z*factorial(c)*(1-u)/(c*u)^c

  # calculation of pns
  if (n < c & n == c){
    return(p0*((c*u)^n)/factorial(n))
  }
  else if (n > c){
    return((u^n) * p0 * (c^c)/factorial(c))
  }
}

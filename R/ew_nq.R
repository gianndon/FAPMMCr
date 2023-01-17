#' ew_nq Function
#'
#' Expected value (average) of objects in the queue
#' @param c number of servers
#' @param utilization utilization of the system
#' @return numeric vector, Returns the expected value of the objects/customers in the QUEUE.
#' @examples ew_nq(c= 2, utilization=0.9)
#' @examples ew_nq(2, 0.9)
#' @export
#'
ew_nq <- function(c, utilization){

  # calculation of zeta
  n <- c-1
  x <- utilization*c
  psi <- (x^n/factorial(n)) / (sum(x^(0:n) / factorial(0:n)))
  z <- (utilization*psi) / (1-utilization+utilization*psi)

  # calculation of expected value of objects in the queue
  return((z*utilization) / (1-utilization))
}

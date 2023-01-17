#' ew_wq Function
#'
#' Average time an object spent in the queue
#' @param c number of servers
#' @param utilization utilization of the system
#' @param mu processing/service rate
#' @return numeric vector, Returns the expected time an objects/customers spent in the QUEUE.
#' @examples ew_wq(c=3, utilization=0.9, mu=3)
#' @examples ew_wq(3, 0.9, 3)
#' @export
#'
ew_wq <- function(c, utilization, mu){

  # zeta calculations
  n <- c-1
  x <- utilization*c
  psi <- (x^n/factorial(n)) / (sum(x^(0:n) / factorial(0:n)))
  z <- (utilization*psi) / (1-utilization+utilization*psi)

  # calculation of expected value of time an object spends in the queue
  return(z / (c*mu*(1-utilization)))
}

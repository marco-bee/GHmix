#' Simulation of a two-population mixture of g-and-h distributions
#'
#' This function simulates a mixture of two g-and-h distributions.
#' @param n real: number of random numbers simulated form the distribution.
#' @param a1 real: location parameter of the first population.
#' @param b1 positive real: scale parameter of the first population.
#' @param g1 real: first shape (skewness) parameter of the first population.
#' @param h1 positive real: second (heavy tailedness) shape parameter of the first population.
#' @param a2 real: location parameter of the second population.
#' @param b2 positive real: scale parameter of the second population.
#' @param g2 real: first shape (skewness) parameter of the second population.
#' @param h2 positive real: second (heavy tailedness) shape parameter of the second population.
#' @return a sample of size n simulated from a mixture of two g-and-h distributions.
#' @export
#' @examples
#' yd <- rghmix(100,0,-1,0.5,.3,.2,1,1,.5,.1)

rghmix <- function(n, p1, a1 , b1, g1, h1, a2, b2, g2, h2)
{
  u <- rbinom(n,1,p1)
  n1 <- sum(u)
  x1 = rgh(n1,a1,b1,g1,h1)
  x2 = rgh(n-n1,a2,b2,g2,h2)
  x <- c(x1,x2)
  return(x)
}

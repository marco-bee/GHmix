#' Density function of a two-population mixture of g-and-h distributions
#'
#' This function computes numerically the density of a mixture of two g-and-h distributions.
#' @param x real: values where the density has to be evaluated.
#' @param a1 real: location parameter of the first population.
#' @param b1 positive real: scale parameter of the first population.
#' @param g1 real: first shape (skewness) parameter of the first population.
#' @param h1 positive real: second (heavy tailedness) shape parameter of the first population.
#' @param a2 real: location parameter of the second population.
#' @param b2 positive real: scale parameter of the second population.
#' @param g2 real: first shape (skewness) parameter of the second population.
#' @param h2 positive real: second (heavy tailedness) shape parameter of the second population.
#' @return value of the density function of a mixture of two g-and-h distributions.
#' @export
#' @examples
#' yd <- dghmix(1,0,-1,0.5,.3,.2,1,1,.5,.1)

dghmix <- function(x,p,a1,b1,g1,h1,a2,b2,g2,h2)
{
  y <- p*dgh(x,a1,b1,g1,h1,0)+(1-p)*dgh(x,a2,b2,g2,h2,0)
  y
}

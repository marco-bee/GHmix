#' Log-likelihood function of the g distribution
#'
#' This function evaluates the log-likelihood function of the g distribution
#' @param g real: shape (skewness) parameter.
#' @param x numerical vector: observations.
#' @return value of the log-likelihood function of the g distribution.
#' @export
#' @examples
#' yd <- lld(1,0.5)

lld <- function(g,x)
{
  f <- dg(x,g)
  l <- sum(log(f))
  return(l)
}

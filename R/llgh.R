#' Log-likelihood function of the g-and-h distribution
#'
#' This function evaluates the log-likelihood function of the standard (a=0, b=1)
#' g-and-h distribution.
#' @param x bivariate vector: values of the g and h parameters.
#' @param y numerical vector: observations.
#' @return value of the log-likelihood function of the g distribution.
#' @export
#' @examples
#' yd <- lld(1,0.5)

llgh <- function(x,y)
{
  a <- 0
  b <- 1
  g <- x[1]
  h <- x[2]
  ll <- dgh(y, a, b, g, h, logar=1)
  return(sum(ll))
}

#' Density function of the g distribution
#'
#' dg evaluates the density of the g distribution
#' @param x real: numerical vector: values where the density has to be evaluated.
#' @param a real: location parameter.
#' @param b positive real: scale parameter.
#' @param g real: shape (skewness) parameter.
#' @return Value of the density function of the g distribution.
#' @export
#' @examples
#' yd <- dg(1,0,1,0.5)

dg <- function(x0,a,b,g)
{
  gammap <- 1/g
  x0 <- (x0-a)/b
  if (g>0)
  {
    x <- x0+gammap
    print(range(x))
    mu <- log(1/g)
    sigma <- g
    f <- dlnorm(x,mu,sigma)/b
    return(f)
  }
  if (g<0)
  {
    x <- rev(-x0+gammap)
    print(range(x))
    mu <- log(1/-g)
    sigma <- -g
    f <- rev(dlnorm(x,mu,sigma))/b
    return(f)
  }
}

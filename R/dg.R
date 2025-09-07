#' Density function of the g distribution
#'
#' This function evaluates the density of the g distribution
#' @param x real: numerical vector: values where the density has to be evaluated.
#' @param g positive real: shape (skewness) parameter.
#' @return value of the density function of the g distribution.
#' @export
#' @examples
#' yd <- dg(1,0.5)

dg <- function(x,g)
{
f <- rep(0,length(x))
gammap <- -1/g
mu <- log(1/g)
sigma <- g
indici <- which(x>gammap)
f[indici] <- dlnorm(x[indici]-gammap,mu,sigma)
return(f)
}

#' Cumulative distribution function of the a g-and-h distribution
#'
#' This function computes the cdf of a g-and-h distribution for a vectorial input.
#' @param x real vector: values where the cdf has to be evaluated.
#' @param a real: location parameter.
#' @param b positive real: scale parameter.
#' @param g real: first shape (skewness) parameter.
#' @param h positive real: second (heavy tailedness) shape parameter.
#' @return value of the cdf of the g-and-h distribution.
#' @export
#' @examples
#' yd <- pgh(1,0,1,0.5,.3,0)

pgh <- function(x, a, b, g, h)
{
    f <- mapply(pgh_scalar, x, a, b, g, h)
    return(f)
}

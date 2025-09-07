#' Cumulative distribution function of the a g-and-h distribution
#'
#' This function computes the cdf of a g-and-h distribution for a scalar input.
#' @param x real: value where the cdf has to be evaluated.
#' @param a real: location parameter.
#' @param b positive real: scale parameter.
#' @param g real: first shape (skewness) parameter.
#' @param h positive real: second (heavy tailedness) shape parameter.
#' @return value of the cdf of the g-and-h distribution.
#' @export
#' @examples
#' yd <- pgh_scalar(1,0,1,0.5,.3,0)

pgh_scalar <- function(x, a, b, g, h)

{
if (h > 0)
{
      toroot = function(p,a,b,g,h,x)
   {
      f <- qgh(p, a, b, g, h) - x
      return(f)
   }
   p = uniroot(toroot, interval = c(0,1), a=a, b=b, g=g, h=h, x=x, check.conv = T, tol=1e-10)
}
if (h == 0)
{
   if (g > 0)
      p = integrate(dg, lower = -1/g, upper = x, g=g)
   if (g<=0)
      p = 0
}
return(p)
}


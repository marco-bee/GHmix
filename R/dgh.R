#' Density function of the g-and-h distribution
#'
#' This function computes numerically the density of a g-and-h distribution.
#' @param x real: values where the density has to be evaluated.
#' @param a real: location parameter.
#' @param b positive real: scale parameter.
#' @param g real: first shape (skewness) parameter.
#' @param h positive real: second (heavy tailedness) shape parameter.
#' @param logar logical: if logar=0, the density is computed, if logar=1, the
#' log-density is computed.
#' @return value of the density function of the g-and-h distribution.
#' @export
#' @examples
#' yd <- dgh(1,0,1,0.5,.3,0)

dgh <- function(x, a, b, g, h, logar)
{
  if (h>0 && g!=0)
  {
    Ff <- unlist(pgh(x,a,b,g,h)[1,])
    px <- qnorm(Ff)
    f <- dnorm(px)/Qgh_deriv(px, b, g, h)
    f[f==0] <- 1e-320 # 1e-320
    if (logar == 1)
      f <- log(f)
    return(as.double(f))
  }
  if (g == 0 && h > 0)
  {
    Ff <- unlist(pgh(x,a,b,g,h)[1,])
    Ff <- pmax(Ff,.01)
    px <- qnorm(Ff)
    f <- dnorm(px)/exp(px^2*h/2)
    f[f==0] <- 1e-320 # 1e-320
    if (logar == 1)
      f <- log(f)
    return(as.double(f))
  }
  if (g == 0 && h == 0)
  {
    f <- dnorm(x,a,b)
    if (logar == 1)
      f <- log(f)
    return(as.double(f))
  }
  if (h==0 && g!=0)
  {
    f <- dg(x,a,b,g)
    f[f==0] <- 1e-320 # 1e-320
    if (logar == 1)
      f <- log(f)
    return(as.double(f))
  }
}

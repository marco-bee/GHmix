#' Quantile function of the a g-and-h distribution
#'
#' This function computes the quantile function of a g-and-h distribution.
#' @param x real, 0<=x<=1: quantile levels.
#' @param a real: location parameter.
#' @param b positive real: scale parameter.
#' @param g real: first shape (skewness) parameter.
#' @param h positive real: second (heavy tailedness) shape parameter.
#' @return value of the quantile function of the g-and-h distribution.
#' @export
#' @examples
#' yd <- qgh(.1,0,1,0.5,.3)

qgh <- function(p,a,b,g,h)
  {
  z <- qnorm(p,0,1)
  if(g==0){
    x <- a + b*z*exp((h*z^2)/2)
  } else if(g==0 & h==0){
    x <- z
  } else{
    x <- a + b*(exp(g*z)-1)*(1/g)*exp((h*z^2)/2)
  }
  return(x)
}

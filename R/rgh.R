#' Random number simulation from a g-and-h distribution
#'
#' This function simulates random numbers from a g-and-h distribution
#' using its stochastic representation
#' @param n positive integer: number of simulated random numbers.
#' @param a real: location parameter.
#' @param b positive real: scale parameter.
#' @param g real: first shape (skewness) parameter.
#' @param h positive real: second (heavy tailedness) shape parameter.
#' @return n iid random numbers from the g-and-h distribution.
#' @export
#' @examples
#' ySim <- rgh(100,1,1,0.5,0.3)

rgh <- function(n,a,b,g,h)
{
  z <- rnorm(n,0,1)
  if(g==0){
    x <- a + b*z*exp((h*z^2)/2)
  } else if(g==0 & h==0){
    x <- z
  } else{
    x <- a + b*(exp(g*z)-1)*(1/g)*exp((h*z^2)/2)
  }
  return(x)
}

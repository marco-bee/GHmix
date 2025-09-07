#' First derivative of the g-and-h distribution quantile function
#'
#' This function evaluates the first derivative of the g-and-h distribution
#' quantile function
#' @param x real: numerical vector: values where the function has to be evaluated.
#' @param b positive real: scale parameter.
#' @param g real: first shape (skewness) parameter.
#' @param h positive real: second (heavy tailedness) shape parameter.
#' @return value of the first derivative of the g-and-h distribution
#' quantile function.
#' @export
#' @examples
#' yf <- Qgh_deriv(1, 1, 0.3, 0.2)

Qgh_deriv <- function(x, b, g, h)
{
  f1 <- b * (exp(g*x+h*(x^2)/2) + (h/g) * x * exp(h*(x^2)/2) * (exp(g*x)-1))
  return(f1)
}

#' Quantile estimation of the g-and-h distribution
#'
#' This function the quantile estimators of the g-and-h distribution
#' @param x sample of observations from a g-and-h distribution.
#' @return quantile estimates of the parameters of the g-and-h distribution.
#' @export
#' @examples
#' ySim <- rgh(100,1,1,0.5,0.3)
#' qPars <- qEst(ySim)

qEst <- function(x){
  # quantile estimator Hoaglin (1985)
  param <- matrix(0,1,4)
  p <- c(0.005,0.01,seq(0.025,0.475,0.025))
  a <- median(x)
  z <- qnorm(p)
  # Add 1e-5 to prevent UHS and LHS to become zero as it creates problem in the log.
  UHS <- quantile(x,1-p) - a + 0.00001
  LHS <- a - quantile(x,p) + 0.00001
  g <- (-1/z) * log(UHS/LHS)
  g <- median(g)
  k <- log((UHS*g)/(exp(-g*z)-1))
  kk <- (z^2/2)
  reg <- lm(k~kk)
  b <- exp(reg$coef[1])
  h <- reg$coef[2]
  param[1] <- a
  param[2] <- b
  param[3] <- g
  param[4] <- h
  return(param)
}

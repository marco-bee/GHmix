#' MLE on samples obtained by resampling with replacement the original data.
#'
#' This function computes the MLEs of the parameters of #' a g-and-h distribution
#' on samples obtained by resampling with replacement the original data.
#' The output is to be used for computing standard errors via non-parametric
#' bootstrap.
#' @param dati (n x 1) vector: observed  sample.
#' @param nome string: name of the file where the output is saved.
#' @return Nothing. The matrix containing the estimated parameters at each
#' bootstrap replication are saved to fule pars.txt.
#' @export

MLEBoot <- function(x,dati,nome)
{
  library(GHmix)
  # source('G:\\Il mio Drive\\OldStuff\\Paperi\\g_vs_gh\\programs\\ld.R')
  # source('G:\\Il mio Drive\\OldStuff\\Paperi\\IndInfgh\\programs\\qEst.R')
  # source('G:\\Il mio Drive\\OldStuff\\Paperi\\IndInfgh\\covariates\\programs\\llghcov.R')
  # source('G:\\Il mio Drive\\OldStuff\\Paperi\\g_vs_gh\\programs\\dg.R')
  samSiz <- length(dati)
  logar <- 1
  indici = sample(samSiz, samSiz, replace = TRUE)
  yboot = dati[indici]
  tempQ <- qEst(yboot)
  ys <- (yboot-tempQ[1])/tempQ[2]
  ymin <- min(ys)
  res1 <- optimize(lld,c(0,-1/ymin),ys,maximum=T)
  MLE1 <- res1$maximum
  res <-  optim(c(MLE1,max(c(tempQ[4],0))), llgh, gr = NULL, method='L-BFGS-B', lower = c(-Inf,0.01), upper = c(Inf,Inf), control = list(fnscale = -1), ys)
  pars <- c(tempQ[1],tempQ[2],res$par[1],res$par[2])
  write(t(pars),paste('parsBoot',nome,'.txt',sep=''),ncolumns = 4,append=TRUE)
}

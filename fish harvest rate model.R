library(deSolve)

grwt <- function(t, n, P) {
  dndt <- P$r0 * n * ( 1 - n / P$K ) - P$h*n
  return(list( dndt ))
}

n0 <- 1
P <- list(r0=1,K=100,h=0.2)
timevec <- seq(0,15,0.1)

out <- ode(y=n0,func=grwt,times=timevec,parms=P)
plot(out)
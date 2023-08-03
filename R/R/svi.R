svi <- function(x, par){
  n <- length(par)
  if(n == 6){
    z <- par/par[2]
    co1 <- z[3] * x + z[5]
    co2 <- z[1] * x^2 + z[4] * x + z[6]
    yhat <- 0.5*(-co1 + sqrt(co1^2 - 4*co2))
  } else if(n == 5){
    xb <- x - par[4]
    yhat <- par[1] + par[2]*par[3]*xb + par[2]*sqrt(xb^2 + par[5]^2)
  } else {
    stop("invalid input parameters (see documentation)")
  }
  return(yhat)
}

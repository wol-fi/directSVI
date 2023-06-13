# axel vogt: par <- c(-0.041, 0.1331, 0.306, 0.3586, 0.4153)
svipar <- function(par){
  n <- length(par)
  if(n == 6){
    z <- par/par[3]
    b2 <- 0.25*z[2]^2 - z[1]*z[3]
    b <- sqrt(b2)
    rho <- z[2]/(-2*b)
    m <- (2*z[3]*z[4] - z[2]*z[5])/(4*b2)
    sig <- sqrt( (0.25*z[5]^2 - z[6])/b2 - m^2)
    a <- 0.5*(-z[2]*m-z[5])
    out <- c(a,b,rho,m,sig)
    names(out) <- c("a", "b", "rho", "m", "sig")
  } else if(n==5){
    a <- par[1]
    b <- par[2]
    r <- par[3]
    m <- par[4]
    s <- par[5]
    tmp <- b*r*m - a
    b2 <- b^2
    out <- c(b2*(r^2-1),
           -2*b*r,
           1,
           2*m*b2 - 2*b*r*tmp,
           2*tmp,
           tmp^2 - b2*(m^2+s^2))
    names(out) <- paste0("z", 1:6)
  } else {
    stop("invalid input")
  }
  return(out)
}

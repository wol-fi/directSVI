# Axel-Vogt:
# par <- c(-0.041, 0.1331, 0.306, 0.3586, 0.4153)
durrleman <- function(x, par, Gs=FALSE){
  n <- length(par)
  if(n == 6){
    par <- svipar(par)
  } else if(n != 5){
    stop("invalid input parameters")
  }

  a <- par[1]
  b <- par[2]
  rho <- par[3]
  m <- par[4]
  sig <- par[5]

  l <- (x-m)/sig
  mu <- m/sig
  k <- sqrt(l^2+1)

  w <- svi(x, par)
  w1 <- b*(rho+l/k)
  w2 <- b/k^3/sig
  # N <- w/sig
  # N1 <- b*(rho+l/k)
  # N2 <- b/k^3
  # tmp <- (l+mu)/(2*N)
  # G1p <- 1 - N1*(tmp + 0.25)
  # G1m <- 1 - N1*(tmp - 0.25)
  # G2 <- N2 - N1^2/(2*N)
  G1p <- 1-w1*(0.5*x/w + 0.25)
  G1m <- 1 - w1*(0.5*x/w - 0.25)
  G1 <- G1m*G1p
  G2 <- b/k^3 - sig*0.5*w1^2/w
  g <- G1 + G2/(2*sig)
  if(Gs){
    ret <- data.frame(g, G1, G2, G1p, G1m)
  } else {
    ret <- g
  }
  return(ret)
}

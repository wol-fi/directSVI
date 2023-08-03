Hmin <- function(z){
  a1 <- z[2]^2-4*z[1]*z[3]
  a2 <- 2*z[2]*z[5]-4*z[3]*z[4]
  a3 <- z[5]^2-4*z[3]*z[6]
  return(a3-a2^2/(4*a1))
}

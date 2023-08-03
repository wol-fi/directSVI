# compute asymptotes, center, angle, etc.
# https://stackoverflow.com/questions/72755640/how-to-fit-a-rotated-and-translated-hyperbola-to-a-set-of-x-y-points-in-python
# https://math.stackexchange.com/questions/1286146/how-to-find-center-of-a-conic-section-from-the-equation
geom <- function(par){
  n <- length(par)
  if(n == 5){
    z <- svipar(par)
  } else if(n != 6){
    stop("invlaid input parameters")
  } else {
    z <- par
  }

  # center:
  gam <- z[2]^2 - 4*z[1]*z[3]
  x0 <- (2*z[3]*z[4] - z[2]*z[5])/gam
  y0 <- (2*z[1]*z[5] - z[2]*z[4])/gam

  # asymptotes:
  kas <- z[1]/(0.5*(-z[2] + c(1,-1)*sqrt(gam)))
  phi <- abs(atan((kas[1]-kas[2])/(1+kas[1]*kas[2]))) * 180/pi

  ret <- c(x0, y0, kas, phi)
  names(ret) <- c("x0", "y0", "slope1", "slope2", "angle")
  return(ret)
}

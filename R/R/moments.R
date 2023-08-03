rnmoment <- function(par, order=2, absolute=FALSE, standardized=TRUE){
  # note: x=moneyness -> E(x) = 0
  if(order == 1 & !absolute){
    mom <- 0
  } else {
    if(absolute){
      mom <- integrate(function(x) abs(x)^order * rnd(x,par), lower = -100, upper = 100,  stop.on.error = FALSE)$value  
    } else {
      mom <- integrate(function(x) x^order * rnd(x,par), lower = -100, upper = 100,  stop.on.error = FALSE)$value        
    }
  }
  
  if(standardized & (order != 2 & !absolute)){
    vol <- sqrt(integrate(function(x) x^2*rnd(x,par), lower = -100, upper = 100,  stop.on.error = FALSE)$value)
    mom <- mom/vol^order
  }
  return(mom)
}

rnvar <- function(par) rnmoment(par)

rnskew <- function(par) rnmoment(par, order=3)

rnkurt <- function(par) rnmoment(par, order=4)
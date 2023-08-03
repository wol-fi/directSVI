
svifit <- function(x, y, fit="direct", na.rm=TRUE, low_ecc=TRUE, W=NA, a=NA, init=c(0, 0.2)){
  nas <- is.na(x) | is.na(y)
  if(na.rm){
    sel <- !nas
    x <- x[sel]
    y <- y[sel]
  } else {
    if(any(nas)) stop("Data includes NA's. Remove or set 'na.rm=TRUE'.")
  }
  n <- length(x)
  if(n != length(y)) stop("x and y are of unequal length!")
  if(is.na(W)){
    W <- rep(1, n)
  } else {
    W <- as.matrix(W)
    if(sum(dim(W) == c(n, 1)) != 2) stop("Weights `W` not properly defined. Must be `NA` or a vector of same length as `x`.")
    W <- as.numeric(W)
  }
  
  if(fit=="direct_UC"){
    D <- cbind(x^2, x*y, x, y, 1)
    mdl <- lm.wfit(D, -y^2, W)
    z <- mdl$coefficients
    z <- c(z[1], 1, z[2:5])
  } else if(fit == "vanish"){
    if(is.na(a)){
      D <- cbind(x*y, x, y, 1)
      mdl <- lm.wfit(X, -y^2, W)
      z <- mdl$coefficients
      z <- c(0, 1, z[1:4])
    } else {
      Y <- -(y-a)^2
      X <- cbind((a-y)*x, y, 1)
      mdl <- lm.wfit(X, Y, W)
      co <- mdl$coefficients
      rho <- sign(co[1])
      b <- abs(co[1])/2
      m <- co[2]/co[1]
      sigma <- sqrt((-a*co[2]-co[3])/b^2)
      par <- c(a, b, rho, m, sigma)
      z <- svipar(par)
    }
  } else if(fit == "direct"){
    D <- cbind(x^2, y^2, x*y, x, y, 1)
    C1 <- rbind(c(0, -0.5), c(-0.5, 0))
    Ci <- rbind(c(0, -2), c(-2, 0))
    S <- crossprod(D, diag(W)) %*% D
    S1 <- S[1:2, 1:2]
    S2 <- S[1:2, 3:6] 
    S3 <- S[3:6, 3:6]
    M0 <- -Matrix::tcrossprod(Matrix::chol2inv(Matrix::chol(S3)), S2)
    M <- (S1 + S2 %*% M0)
    E <- eigen(Ci %*% M)
    evec <- E$vectors
    evec[,1] <- evec[,1]/evec[2,1]
    evec[,2] <- evec[,2]/evec[2,2]
    zh2 <- evec[,evec[1,] < 0]
    zh <- c(zh2, M0 %*% zh2)
    if(low_ecc){
      ze2 <- evec[,evec[1,] > 0]
      ze <- c(ze2, M0 %*% ze2)
      Dx <- cbind(2*x, 0, y, 1, 0, 0)
      Dy <- cbind(0, 2*y, x, 0, 1, 0)
      Sxy <- crossprod(Dx,Dx) + crossprod(Dy,Dy)
      sig <- c(crossprod(ze, S) %*% ze,
               crossprod(ze, S) %*% zh,
               crossprod(zh, S) %*% zh,
               crossprod(ze, Sxy) %*% ze,
               crossprod(ze, Sxy) %*% zh,
               crossprod(zh, Sxy) %*% zh)
      Q2 <- sig[2]*(sig[4]-sig[6])+sig[3]*(sig[5]-sig[4])+sig[1]*(sig[6]-sig[5])
      Q1 <- sig[1]*(2*sig[5]-sig[6])+sig[4]*(sig[3]-2*sig[2])
      Q0 <- sig[2]*sig[4]-sig[1]*sig[5]
      mu_12 <- (-Q1 + c(1,-1)*sqrt(Q1^2 - 4*Q2*Q0))/(2*Q2)
      cond <- function(mu){
        z <- mu*zh + (1-mu)*ze
        a <- z[3]^2 - 4*z[1]
        b <- 2*z[3]*z[5] - 4*z[4]
        C <- z[5]^2 - 4*z[6]
        4*a*C - b^2
      }
      mu_min <- uniroot(cond, c(0,10^3))$root+1e-4
      mu <- max(mu_min, max(mu_12), 1)
      z <- (1-mu)*ze + mu*zh
    } else {
      z <- zh
    }
  } else if(fit == "QE"){
    # quasi explicit
    fqe <- function(par, x, y){
      xb <- x - par[1]
      xbb <- sqrt(xb^2 + par[2]^2)
      mdl0 <- lm.fit(cbind(1, xb, xbb), y)
      return(mdl0)
    }
    mdl1 <- optim(init, function(par, x=x,y=y) sum(fqe(par,x=x, y=y)$residuals^2), 
                 x=x, y=y, lower=c(-Inf, 0), method="L-BFGS-B")
    par1 <- mdl1$par
    mdl2 <- fqe(par1, x, y)
    par2 <- mdl2$coefficients  
    par <- c(par2[1], par2[3], par2[2]/par2[3], par1[1], par1[2])
    z <- svipar(par)
  } else { 
    stop("Fitting method not properly defined.")
  }
  yhat <- svi(x, z)
  par <- svipar(z)
  ret <- list(yhat=yhat, par=par, z=z, input=cbind(x,y))
  class(ret) <- "svi"
  return(ret)
}

predict.svi <- function(object, x=NULL, ...){
  if(is.null(x)){
    x <- object$input[,1]
  }
  par <- object$par
  xb <- x - par[4]
  w <- par[1] + par[2]*(par[3]*xb + sqrt(xb^2 + par[5]))
  return(w)
}

plot.svi <-function(fit, extrap=1, ...){
  df <- fit$input
  x <- df[,1]
  if(extrap != 1){
    mm <- extrap*c(min(x), max(x))
    xn <- seq(mm[1], mm[2], length.out=round(length(x)*1.5))
    yh <- predict(fit, xn)
  } else {
    yh <- fit$yhat
    xn <- x
  }
  plot(xn, yh, type="l")
  points(df)
}






svifit <- function(x, ivol, iterative=TRUE, flat_exp=FALSE, na.rm=TRUE, W=NA, a=NA){
  y <- ivol^2
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
  if(is.na(W[1])){
    W <- rep(1, n)
  } else {
    W <- as.matrix(W)
    if(sum(dim(W) == c(n, 1)) != 2) stop("Weights `W` not properly defined. Must be `NA` or a vector of same length as `x`.")
    W <- as.numeric(W)
  }
  
  if(flat_exp){
    if(is.na(a)){
      D <- cbind(x*y, x, y, 1)
      mdl <- lm.wfit(D, -y^2, W)
      z <- mdl$coefficients
      z <- c(0, 1, z[1:4])
      par <- svipar(z)
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
    }
  } else {
    D <- cbind(x^2, y^2, x*y, x, y, 1)
    S <- crossprod(D, diag(W)) %*% D
    S1 <- S[1:2, 1:2]
    S2 <- S[1:2, 3:6] 
    S3 <- S[3:6, 3:6]
    M0 <- -Matrix::tcrossprod(Matrix::chol2inv(Matrix::chol(S3)), S2)
    M <- (S1 + S2 %*% M0)
    zh2 <- c(-sqrt(M[2,2]/M[1,1]),1)
    zh <- c(zh2, M0 %*% zh2)
    
    if(iterative){
      z <- zh
      b2 <- 0.25*z[3]^2 - z[1]
      rho <- z[3]/(-2*sqrt(b2))
      m <- (2*z[4] - z[3]*z[5])/(4*b2)
      sig <- (0.25*z[5]^2 - z[6])/b2 - m^2
      if(sig < 0){
        sig <- 0.2
      } else {
        sig <- sqrt(sig)
      }
      fqe <- function(par){
        xb <- x - par[1]
        xbb <- sqrt(xb^2 + par[2]^2)
        X <- cbind(1, xb, xbb)
        mdl0 <- lm.wfit(X, y, w = W)
        return(mdl0)
      }
      mdl1 <- optim(c(m, sig), function(par, x=x,y=y) sum(fqe(par)$residuals^2), 
                    lower=c(-Inf, 0), method="L-BFGS-B")
      par1 <- mdl1$par
      mdl2 <- fqe(par1)
      par2 <- mdl2$coefficients  
      par <- c(par2[1], par2[3], par2[2]/par2[3], par1[1], par1[2])
    } else {
      # low eccentricity correction if non-iterative
        ze2 <- c(-zh2[1],1)
        ze <- c(ze2, M0 %*% ze2)
        d <- zh-ze
        e <- ze
        Dx <- cbind(2*x, 0, y, 1, 0, 0)
        Dy <- cbind(0, 2*y, x, 0, 1, 0)
        Sxy <- Matrix::crossprod(Dx,Dx) + Matrix::crossprod(Dy,Dy)
        Z <- cbind(ze, zh)
        sig <- (Matrix::crossprod(Z, S) %*% Z)[c(1,3,4)]
        sig <- c(sig, (Matrix::crossprod(Z, Sxy) %*% Z)[c(1,3,4)])
        Q0 <- sig[2]*sig[4]-sig[1]*sig[5]
        Q1 <- -2*Q0 + sig[3]*sig[4] - sig[1]*sig[6]
        Q2 <- sig[3]*sig[5] - sig[2]*sig[6] - Q0 - Q1

        mu_12 <- (-Q1 + c(1,-1)*sqrt(Q1^2 - 4*Q2*Q0))/(2*Q2)
        s <- c(t(d)%*%S%*%d, 2*t(d)%*%S%*%e, t(e)%*%S%*%e)
        sxy <- c(t(d)%*%Sxy%*%d, 2*t(d)%*%Sxy%*%e, t(e)%*%Sxy%*%e)
        MU <- cbind(c(mu_12[1]^2, mu_12[1], 1), c(mu_12[2]^2, mu_12[2], 1))
        if((s %*% MU[,1] / (sxy %*% MU[,1])) < (s %*% MU[,2] / (sxy %*% MU[,2]))) mu_12 <- mu_12[c(2,1)] # mu_12[1] = maximizer, mu_12[2] = minimizer

        co <- c(d[3]*d[4]*d[5] - d[1]*d[5]^2 - d[6]*d[3]^2,
                e[3]*d[4]*d[5] + d[3]*e[4]*d[5] + d[3]*d[4]*e[5] - d[5]^2*e[1]-2*d[1]*d[5]*e[5] - d[3]^2*e[6]-2*d[6]*d[3]*e[3] + 4*d[1]*d[6] - d[4]^2,
                e[3]*e[4]*d[5]+e[3]*d[4]*e[5]+d[3]*e[4]*e[5] - d[1]*e[5]^2 - 2*d[5]*e[1]*e[5]-d[6]*e[3]^2 - 2*d[3]*e[6]*e[3] + 4*d[6]*e[1]-2*d[4]*e[4]+4*d[1]*e[6],
                e[3]*e[4]*e[5] - e[1]*e[5]^2 - e[6]*e[3]^2 + 4*e[1]*e[6] - e[4]^2)
        p <- polynomial(rev(co))
        mu0 <- solve(p)
        mu0[mu0 < 0.5] <- 0.5
        mu0 <- sort(mu0)

        mu <- max(0.5, mu0[3], mu_12[2])
        z <- (1-mu)*ze + mu*zh
        par <- svipar(z)
    }
        
  }
  
  xb <- x - par[4]
  yhat <- sqrt(par[1] + par[2]*par[3]*xb + par[2]*sqrt(xb^2 + par[5]^2))
  
  names(par) <- c("a", "b", "rho", "m", "sigma")
  fit <- list(data=data.frame(x=x, y=y), yhat=yhat, par=par, input=cbind(x,ivol), resid=yhat-ivol)
  class(fit) <- "svi"
  return(fit)
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
  df <- fit$data
  x <- df$x
  dx <- mean(abs(diff(x)))
  xn <- seq(min(x)-3*dx, max(x)+3*dx, dx/5)
  yh <- sqrt(svi(xn, fit$par))
  plot(xn, yh, type="l")
  points(x, sqrt(df$y))
}





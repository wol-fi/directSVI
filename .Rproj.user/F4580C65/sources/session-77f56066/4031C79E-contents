
svifit <- function(x, y, C="shape", a=NA, W=NA, na.rm=TRUE){

  if(length(x) != length(y)) stop("x and y are of unequal length!")
  nas <- is.na(x) | is.na(y)
  if(na.rm){
    sel <- !nas
    x <- x[sel]
    y <- y[sel]
  } else {
    if(any(nas)) stop("Data includes NA's. Remove or set 'na.rm=TRUE'.")
  }
  if(length(W)==1) W <- rep(1, length(x))

  if(C[1] == "none"){
    X <- cbind(x^2, x*y, x, y, 1)
    mdl <- lm.wfit(X, -y^2, W)
    z <- mdl$coefficients
    z <- c(z[1:2], 1, z[3:5])
  } else if(C[1] == "vanish"){
    if(is.na(a)){
      X <- cbind(x*y, x, y, 1)
      mdl <- lm.wfit(X, -y^2, W)
      z <- mdl$coefficients
      z <- c(0, z[1], 1, z[2:4])
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

  } else {
    W <- diag(W)
    if(C[1]=="shape"){
      sel1 <- 1:3
      sel2 <- 4:6
      C1 <- rbind(c(0,0,-2), c(0,1,0), c(-2,0,0))
      Ci <- rbind(c( 0, 0,-0.5), c( 0, 1, 0), c(-0.5, 0, 0))
    } else if(C[1]=="refined"){
      sel1 <- c(1,3)
      sel2 <- c(2,4:6)
      C1 <- rbind(c(0, -0.5), c(-0.5, 0))
      Ci <- rbind(c(0, -2), c(-2, 0))
    } else {
      if(!min(dim(C)==c(6,6))) stop("Constraint matrix not properly defined. Should be of Dimension 6x6.")
      # individual C, detect where the constraint enters
      sel0 <- (colSums(C==0)<6 | rowSums(C==0)<6)
      sel1 <- (1:6)[sel0]
      sel2 <- (1:6)[!sel0]
      C1 <- C[sel1,sel1]
      Ci <- solve(C1)
    }
    # design matrix
    D <- cbind(x^2, x*y, y^2, x, y, 1)
    S <- t(D) %*% W %*% D
    S1 <- S[sel1, sel1]
    S2 <- S[sel1, sel2]
    S3 <- S[sel2, sel2]
    M0 <- -solve(S3) %*% t(S2)
    M <- (S1 + S2 %*% M0)

    # eigen-decomposition
    if(is.null(Ci)){
      E <- eigen(solve(M) %*% C1)
      lambda <- 1/E$values
    } else {
      E <- eigen(Ci %*% M)
      lambda <- E$values
    }
    evec <- E$vectors
    cond <- diag(t(evec)%*%C1%*%evec)
    lambda[cond < 0] <- NA
    zh2 <- evec[,which.min(lambda)]
    zh <- c(zh2, M0 %*% zh2)
    z <- zh
    z <- z[sort(c(sel1, sel2), index.return=T)$ix]
    z <- z/z[3]

  }
  yhat <- svi(x, z)
  par <- svipar(z)
  ret <- list(yhat=yhat, par=par, z=z/z[3], input=cbind(x,y))
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

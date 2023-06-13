\name{svi}
\alias{svi}
\title{Calculate SVI predicted values}
\usage{
  svi(x, par)
}
\description{
  Calculates the \eqn{y} values as predicted by SVI for some given \eqn{x} and parameter set.
}
\arguments{
  \item{x}{Values of forward log moneyness. I.e., log(strike/forward).}
  \item{par}{Either the five SVI parameters, or the six \eqn{z} coefficients defining the conic.}
}
\details{
  ...
}
\value{Returns the predicted \eqn{y} values.}
\examples{
## Not run
# load("sp500")
# df <- opt$`tau = 7 days`
# x <- df$k
# y <- df$ivol^2
# fit <- svifit(x, y)
# plot(x, y)
# lines(x, fit$yhat)
}

\seealso{\code{\link{svipar}}}

\references{...}

\keyword{svi, implied volatility}

\note{...}

\author{Wolfgang Schadner}

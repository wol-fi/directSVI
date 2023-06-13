\name{svifit}
\alias{svifit}
\title{Fitting SVI volatilities}
\usage{
  svifit(x, y, C=NULL, opt=NULL)
}
\description{
  Fits the Stochastic Volatility Inspired (SVI) equation to an implied volatility surface    of given maturity. The input variable \code{x} represents the log-moneyness (logarithm of strike over forward-price) and \code{y} the implied variance (implied volatility squared).
}
\arguments{
  \item{x}{Vector of forward log-moneyness. That is, \code{log(strike/forward)}}
  \item{y}{Vector of implied variances corresponding to \code{x}. That is, the squared implied volatilities over \code{x}.}
  \item{C}{Constraint matrix. If unspecified, then optimal solution is constrained to hyperbola shape.}
  \item{opt}{Additional options.}
}
\details{
  ...
}
\value{
  Returns a list of the following:
  \item{yhat}{fitted \code{y} values}
  \item{par}{the SVI parameters "a, b, rho, m, sigma"}
  \item{arb_free}{test whether the fitted model is free of arbitrage}
}
\examples{
## Not run
# library(svi)
# data(sp500)
# df <- sp500$`tau = 7 days`
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

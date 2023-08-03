\name{svifit}
\alias{svifit}
\title{Fitting SVI volatilities}
\usage{
  svifit(x, y, fit="direct", na.rm=TRUE, low_ecc=TRUE, W=NA, a=NA, init=c(0, 0.2))
}
\description{
  Fits the Stochastic Volatility Inspired (SVI) equation to an implied volatility surface of given maturity. The input variable \code{x} represents the log-moneyness (logarithm of strike over forward-price) and \code{y} the implied variance (implied volatility squared).
}
\arguments{
  \item{x}{Vector of option's log moneyness. Preferably defined as \deqn{x = log(\frac{strike\; price}{forward\; price})}}
  \item{y}{Vector of implied variances corresponding to \code{x}. That is, the squared implied volatilities over \code{x}.}
  \item{fit}{Fitting method to be used. Possible vbalues are: 
  \itemize{
    \item \code{direct}: direct least-squares
    \item \code{direct_UC}: direct unconstrained least-squares
    \item \code{vanish}: vanishing wings, i.e. flat extrapolation
    \item \code{QE}: quasi-explicit fit, i.e. iterative routine
  }}
  \item{na.rm}{Removing \code{NA}'s}
  \item{low_ecc}{Correction for the low eccentricity bias.}
  \item{W}{(optional) Weighting vector for a weighted least-squares.}
  \item{a}{(optional) For \code{vanish} fit: specify the y-level for the flat extrapolation.}
  \item{init}{(optional) For \code{QE} fit: Starting values of \eqn{m} and \eqn{\sigma}.}
}
\details{
  The method is implemented as described by \href{https://github.com/wol-fi/directSVI/blob/main/directSVI.pdf}{Schadner (2023)}. 
}
\value{
  Returns a list of the following:
  \item{yhat}{Fitted \code{y} values.}
  \item{par}{The SVI parameters \eqn{a, b, \rho, m, \sigma}.}
  \item{z}{The conic coefficients defining the hyperbola.}
  \item{input}{The input data.}
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
#
# # Risk-Neutral Density:
# q <- rnd(x, fit$par)
# plot(x, q)
}

\seealso{\code{\link{svipar}}}

\references{\href{https://github.com/wol-fi/directSVI/blob/main/directSVI.pdf}{Schadner, W. (2023): Direct Fit for SVI Implied Volatilities}}

\keyword{svi, implied volatility}

\author{Wolfgang Schadner}

\name{rnmoment}
\alias{rnmoment}
\alias{rnskew}
\alias{rnkurt}
\alias{rnvar}

\title{Risk-Neutral Moments}
\usage{
  rnskew(par)
  rnkurt(par)
  rnmoment(par, order=2, absolute=FALSE, standardized=TRUE)
}
\description{
  Computes the risk-neutral variance (\code{rnvar}), skewness (\code{rnskew}), kurtosis (\code{rnkurt}) or more generally a moment of certain order (\code{rnmoment}) for the risk-neutral density distribution as specified by the SVI parameters \code{par}.
}
\arguments{
  \item{par}{either the five SVI parameters (\eqn{a, b, \rho, m, \sigma}) or the conic coefficients \eqn{z}.}
  \item{order}{order of the moment to be computed, the default is to compute the second moment, i.e., the variance.}
  \item{absolute}{a logical value indicating whether absolute moments are to be computed.}
  \item{standardized}{a logical value indicating whether standardized moments are to be computed.}
}
\value{Returns the risk-neutral moment.}
\examples{
## Not run
# data(sp500)
# df <- sp500$`tau = 7 days`
# x <- df$k
# y <- df$ivol^2
# mdl <- svifit(x,y)
# rnskew(mdl$par)
# rnmoment(mdl$par)
}

\seealso{\code{\link{svi}}, \code{\link{rnd}}}

\references{\href{https://github.com/wol-fi/directSVI/blob/main/directSVI.pdf}{Schadner, W. (2023): Direct Fit for SVI Implied Volatilities}}

\keyword{svi, implied volatility}

\author{Wolfgang Schadner}

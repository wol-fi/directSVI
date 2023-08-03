\name{rnd}
\alias{rnd}
\title{Risk-Neutral Density}
\usage{
  rnd(x, par)
}
\description{
  Computes the risk-neutral density distribution for some SVI parameters \code{par}.
}
\arguments{
  \item{x}{Vector of option's log moneyness. Preferably defined as \deqn{x = log(\frac{strike\; price}{forward\; price})}}
  \item{par}{Either the five SVI parameters (\eqn{a, b, \rho, m, \sigma}) or the conic coefficients \eqn{z}.}
}
\value{Returns the risk-neutral density distribution.}
\examples{
## Not run
# axel()
# q <- rnd(x, par)
# plot(x, q)
}

\seealso{\code{\link{svi}}}

\references{\href{https://github.com/wol-fi/directSVI/blob/main/directSVI.pdf}{Schadner, W. (2023): Direct Fit for SVI Implied Volatilities}}

\keyword{svi, implied volatility}

\author{Wolfgang Schadner}

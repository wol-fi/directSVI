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
  \item{x}{Values of forward log moneyness. I.e., log(strike/forward).}
  \item{par}{Either the five SVI parameters, or the six \eqn{z} coefficients defining the conic.}
}
\value{Returns the risk-neutral density distribution.}
\examples{
## Not run
# axel()
# q <- rnd(x, par)
# plot(x, q)
}

\seealso{\code{\link{svi}}}

\references{...}

\keyword{svi, implied volatility}

\author{Wolfgang Schadner}

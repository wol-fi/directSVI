\name{durrleman}
\alias{durrleman}
\title{Durrleman Condition}
\usage{
  durrleman(x, par)
}
\description{
  Calculates the the Durrleman condition of arbitrage.
}
\arguments{
  \item{x}{Values of forward log moneyness. I.e., log(strike/forward).}
  \item{par}{Either the five SVI parameters, or the six \eqn{z} coefficients defining the conic.}
}
\details{
  The Durrleman condition should be positive across all \eqn{x} for an abritrage free volatility surface.
}
\value{Returns the values of the Durrleman function.}
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

\seealso{\code{\link{svi}}}

\references{...}

\keyword{svi, implied volatility}

\author{Wolfgang Schadner}

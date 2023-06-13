\name{geom}
\alias{geom}
\title{Geometric Properties}
\usage{
  geom(par)
}
\description{
  Calculates the center and asymptotes of the SVI hyperbola.
}
\arguments{
  \item{par}{Either the five SVI parameters, or the six \eqn{z} coefficients defining the conic.}
}
\details{
  Calculates the center and asymptotes of the SVI hyperbola.
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

\name{durrleman}
\alias{durrleman}
\title{Durrleman Condition}
\usage{
  durrleman(x, par, Gs=FALSE)
}
\description{
  Calculates the the Durrleman condition of arbitrage.
}
\arguments{
  \item{x}{Vector of option's log moneyness. Preferably defined as \deqn{x = log(\frac{strike\; price}{forward\; price})}}
  \item{par}{Either the five SVI parameters (\eqn{a, b, \rho, m, \sigma}) or the conic coefficients \eqn{z}.}
  \item{Gs}{Returns additional values of \eqn{G_{1\pm}} and \eqn{G_2}, see Martini and Mingone (2022) for details.}
}
\details{
  The Durrleman function \eqn{g(x)} should be positive across all \eqn{x} for an abritrage free volatility surface. If \eqn{g(x)} is positive, then the risk-neutral density \eqn{q(x)} is also positive. Let \eqn{y} be the implied variance for the moneyness \eqn{x}, then the durrleman function is defined as: \deqn{
    g(x) := (1 - \frac{xy'}{2y})^2-\frac{y'^2}{4}(\frac{1}{y}+\frac{1}{4})+\frac{y''}{2}
  }
  Alternatively, this might be expressed as:\deqn{
    g(x) = G_1(x) + \frac{1}{2\sigma}G_2(x)
  }
  (cp. Martini and Mingone (2022)).
}
\value{Returns the values \eqn{g} of the Durrleman function.}
\examples{
## Not run
# axel()
# g <- durrleman(x, par)
# plot(x, g); abline(h=0)
}

\seealso{\code{\link{svi}}}

\references{Martini, C. and Mingone, A. (2022): \emph{No Arbitrage SVI}, SIAM J. Financial Math., 13(1), pp. 227-261}

\keyword{svi, implied volatility}

\author{Wolfgang Schadner}

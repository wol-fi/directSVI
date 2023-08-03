\name{axel}
\alias{axel}
\title{Axel-Vogt Parameters}
\usage{
  axel()
}
\description{
  Loads the Axel-Vogt parameters, which are a typical example of arbitrage SVI surface.
}

\details{
  The paramters are given as:
  \deqn{-0.041, 0.1331, 0.306, 0.3586, 0.4153}
  corresponding to \eqn{(a, b, rho, m, sigma)}. When inspecting the Durrleman condition,   one recognizes that this setting is not entirely positive.
}
\value{Loads the parameters \code{par} and shows the Durrleman/SVI plot.}
\examples{
## Not run
# axel()
}

\seealso{\code{\link{svi}}}

\references{\href{https://github.com/wol-fi/directSVI/blob/main/directSVI.pdf}{Schadner, W. (2023): Direct Fit for SVI Implied Volatilities}}

\keyword{svi, implied volatility}

\author{Wolfgang Schadner}

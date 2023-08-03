\name{svipar}
\alias{svipar}
\title{Translating conic coefficients into SVI parameters, and vice versa}
\usage{
  svipar(par)
}
\description{This function transforms the 6 conic coefficients \code{z} into the 5 SVI paramters, or other way around.}
\arguments{
  \item{par}{Either the six conic coefficients \code{z}, or the 5 SVI paramters: \eqn{(a b \rho m \sigma)}.}
}
\details{
  A conic is generally defined as:
  \deqn{0 = Dz}
  for the coefficients \eqn{z} and the design matrix \eqn{D}
  \deqn{D = (x^2 xy y^2 x y 1)}
  . There exists a direct mapping between \code{z} and the 5 SVI parameters, see Schadner 2023.
}
\value{Returns either the five SVI paramters:
  \deqn{par = (a b \rho m \sigma)}
  Or the \code{z} coefficients.
  }

\examples{
## Not run
# TBD
}

\seealso{\code{\link{svipar}}}

\references{\href{https://github.com/wol-fi/directSVI/blob/main/directSVI.pdf}{Schadner, W. (2023): Direct Fit for SVI Implied Volatilities}}

\keyword{svi, implied volatility}

\author{Wolfgang Schadner}

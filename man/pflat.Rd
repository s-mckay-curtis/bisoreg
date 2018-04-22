\name{pflat}
\alias{pflat}
\alias{pflat.biso}
\alias{pflat.default}
\title{ Posterior probability of a flat regression curve}
\description{
Compute the posterior probability of a flat regression function.
}
\usage{
pflat(obj, \dots)
\method{pflat}{default}(obj,\dots)
\method{pflat}{biso}(obj,\dots)
}
\arguments{
  \item{obj}{a \code{biso} object}
  \item{\dots}{not used}
}
\details{No details.}
\value{
Returns the average of the postrior draws where \eqn{u_1=\cdots=u_M}{u[1]=...=u[M]}
}
\references{No references.}
\author{S. McKay Curtis}
\note{No further notes.}
\seealso{\code{\link{bisoreg}}}
\examples{
## See examples for bisoreg
}
\keyword{smooth}

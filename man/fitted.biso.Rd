\name{fitted.biso}
\alias{fitted.biso}
\title{ Compute fitted values for a biso object}
\description{
  Computes pointwise posterior medians of the fitted Bernstein regression function
}
\usage{
%fitted.biso(obj, xnew, \dots)
\method{fitted}{biso}(object, xnew, \dots)
}
\arguments{
  \item{object}{ a \code{biso} object}
  \item{xnew}{ values of \code{x} at which to compute posterior median of the fuction values. If missing, the orginal \code{x} vector is used. }
  \item{\dots}{ not used }
}
\details{
None.
}
\value{
A vector of posterior medians.
}
\references{None.}
\author{S. McKay Curtis}
\note{None.}
\seealso{\code{\link{bisoreg}}}
\examples{
## See examples for bisoreg
}
\keyword{smooth}

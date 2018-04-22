\name{loess.wrapper}
\alias{loess.wrapper}
\title{Compute a loess fit using cross validation}
\description{
Uses the \code{crossval} function from the \code{boostrap} package to compute
the optimal value of the tuning parameter in the \code{loess} function
and returns the corresponding \code{loess} fit.
}
\usage{
loess.wrapper(x, y, span.vals = seq(0.25, 1, by = 0.05), folds = 5)
}
\arguments{
  \item{x}{predictor values}
  \item{y}{response values}
  \item{span.vals}{values of the tuning parameter to evaluate using
    cross validation}
  \item{folds}{number of "folds" for the cross-validation procedure}
}
\details{None.}
\value{Returns a \code{loess} object.}
\references{Chambers, J. M. (1991) \emph{Statistical Models in S}.}
\author{S. McKay Curtis}
\note{None.}
\seealso{\code{\link[stats]{loess}}}
\examples{
## See examples for bisoreg ##
}
\keyword{smooth}

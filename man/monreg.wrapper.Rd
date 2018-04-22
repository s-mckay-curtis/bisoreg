\name{monreg.wrapper}
\alias{monreg.wrapper}
\title{Fit a monotonic regression function using monreg and cross validation}
\description{
Uses cross validation to select the optimal value of a tuning parameter
in the monreg function.
}
\usage{
% monreg.wrapper(x, y, hr.vals = seq(0.01, 0.4, l = 40), x0=x, folds = 5)
monreg.wrapper(x, y, hr.vals = seq(0.01, 0.4, l = 40), x0, folds = 5)
}
\arguments{
  \item{x}{ predictor values }
  \item{y}{ response values }
  \item{hr.vals}{ values of the tuning parameter to evaluate using cross validation}
  \item{x0}{\code{x} values at which to compute fits.  If missing, \code{x0} is set to \code{x} }
  \item{folds}{ number of folds to use in the cross-validation procedure}
}
\details{None.}
\value{Returns a \code{monreg}.}
\references{Dette, H., Neumeyer, N., Pilz, K. F. (2006). "A simple nonparametric estimator of a strictly monotone regression function." \emph{Bernoulli} 12(3), 469-490. }
\author{S. McKay Curtis}
\note{None.}
\section{Warning}{As of the current version of the \code{srbern} package, the \code{monreg} function in the \code{monreg} package has a memory leak in its \code{C} code.  Calling the \code{monreg} function multiple times (e.g. in a simulation) will eventually consume all of the RAM on your computer and crash \code{R}.}
\seealso{\code{\link[monreg]{monreg}}}
\examples{
## See examples for bisoreg ##
}
\keyword{smooth}

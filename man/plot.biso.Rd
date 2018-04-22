\name{plot.biso}
\alias{plot.biso}
\title{ Plot a biso object}
\description{
Plots the data and the fit a Bayesian monontoic regression function (with optional confidence bands)
}
\usage{
\method{plot}{biso}(x, xnew, cl = TRUE, add = FALSE, color ="red", \dots)
}
\arguments{
  \item{x}{ a \code{biso} object}
  \item{xnew}{ \code{x} values at which to plot the regression fit. If missing, the function uses the orginal \code{x} values }
  \item{cl}{ plot confidence bands (default is \code{TRUE}) }
  \item{add}{ add a line to the existing plot rather than create a new plot (default is \code{FALSE})}
  \item{color}{ color of the fitted line (default is red)}
  \item{\dots}{ other options to pass to the plotting functions}
}
\details{None.}
\value{Returns a plot.}
\references{None.}
\author{S. McKay Curtis}
\note{None.}
\seealso{\code{\link{bisoreg}}}
\examples{
## See examples for bisoreg
}
\keyword{smooth}

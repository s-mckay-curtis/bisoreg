\name{aveslope}
\alias{aveslope}
\title{Average slope of a Bernstein polynomial regression function}
\description{This function returns the posterior distribution of the average slope of the Bernstein polynomial regression function over a given interval.}

\usage{
aveslope(obj, a, b)
}

\arguments{
  \item{obj}{an object returned by the \code{bisoreg} function}
  \item{a}{ lower bound for the interval on which to calculate the slope}
  \item{b}{ upper bound for the interval on which to calculate the slope}
}

\details{
  See \emph{A variable selection approach to Bayesian monotonic regression for details.}
}

\value{
  Returns a vector of average slopes for each MCMC iteration in \code{obj}
}

\references{
  A variable selection approach to Bayesian monotonic regression.
}

\author{
  S. McKay Curtis
}

\seealso{
  \code{\link{bisoreg}}
}

\examples{
\dontrun{
data(childgrowth)
n.thin <- 10
n.bi <- n.thin*1000
n.iter <- n.thin*15000 + n.bi
out <- gibbsiso(childgrowth$day,childgrowth$height,m=40,nmc=n.iter,nbi=n.bi,thin=n.thin)
plot(density(aveslope(out,75,150)))
}
}
\keyword{smooth}
\keyword{nonparametric}

\name{summary.biso}
\alias{summary.biso}

\title{
  Summary of a biso Object
}

\description{
  Returns a matrix of summary statistics for the MCMC draws in a \code{biso} object.
}

\usage{
\method{summary}{biso}(object, ...)
}

\arguments{
  \item{object}{
    a \code{biso} object.
  }
  \item{\dots}{
    not implemented.
  }
}

\details{
  None.
}

\value{
  Returns a matrix with columns for the mean and five number summary of all the parameters in \code{postdraws} element of the \code{biso} object \code{x}.  There is also an additional column for the probability that each \code{u} parameter is equal to zero.
}

\references{
  None.
}

\author{
  S. McKay Curtis
}

\note{
  None.
}

\seealso{
  \cite{\link{bisoreg}}
}

\examples{
## See examples for bisoreg ##
}

\keyword{ methods }

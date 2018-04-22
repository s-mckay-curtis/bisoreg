\name{as.mcmc.biso}
\alias{as.mcmc.biso}

\title{
  Coerce biso Object to mcmc Object
}

\description{
  Coerces a \code{biso} object to an \code{mcmc} object for analysis with the \code{coda} package.
}

\usage{
\method{as.mcmc}{biso}(x, \dots)
}

\arguments{
  \item{x}{ a \code{biso} object. }
  \item{\dots}{ unused }
}

\value{
  An S3 object with class \code{mcmc}.
}

\author{
  S. McKay Curtis
}

\seealso{
  \pkg{coda} package.
}

\keyword{ methods }

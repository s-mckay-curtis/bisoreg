\name{nd2004}
\alias{nd2004}
\title{
  Compute Bayesian monotone regression function of Neelon and Dunson (2004)
}
\description{
  Returns posterior draws from the monotone regression procedure of Neelon and Dunson (2004) using OpenBUGS
}
\usage{
nd2004(x, y, M, n.sim = 5000, n.thin = 1, n.burn = 1000)
}
\arguments{
  \item{x}{predictor values}
  \item{y}{response values}
  \item{M}{number of knots}
  \item{n.sim}{number of MCMC iterations in the final MCMC sample (i.e., iterations kept after thinning and burn-in period)}
  \item{n.thin}{number of iterations to "thin" out of the MCMC chain after burn in}
  \item{n.burn}{number of iterations to burn}
}
\details{
Uses the \code{openbugs} function in the \code{R2WinBUGS} package to compute the posterior draws.  The chains do not converge, however, as indicated by the poor Gelman-Rubin diagnostic.
}
\value{
A \code{bugs} object.
}
\references{Neelon, B. and Dunson, D.B. (2004). "Bayesian isotonic regression and trend analysis." \emph{Biometrics} 60, 398--406.}
\author{S. McKay Curtis}
\seealso{\code{\link[R2WinBUGS]{openbugs}}}
\examples{
\dontrun{
data(childgrowth)
out <- nd2004(childgrowth$day, childgrowth$height, 10)
plot(out)
}
}


\name{bisoreg}
\alias{bisoreg}
\title{
  Bayesian Isotonic Regression
}

\description{
  Computes MCMC simulations from the poseterior distribution of the
  monotonic Bernstein polynomial regression function
}

\usage{
bisoreg(x, y, m = floor(n/2), priors = list(), inits = list(),
        n.sim = 5000, n.burn = 1000, n.thin = 10, seed)
}

\arguments{
  \item{x}{vector of covariate values}
  \item{y}{vector of response values}
  \item{m}{positive integer specifying the order of the Bernstein polynomial basis expansion. Default is half the number of observations in the data set.  This value is reset with \code{m <- floor(m)} to prevent noninteger values from being used.}
  \item{priors}{list of values of prior parameters. Values in the list must be named one of (with default values in parentheses) \code{a.sig} (0.1), \code{b.sig} (0.1), \code{a.tau} (1), \code{b.tau} (1), \code{a.p} (1), \code{b.p} (1), \code{m0} (0), \code{ssq0} (1000).}
  \item{inits}{list of initial values for model parameters.  Values in the list must be named one of \code{u0}, \code{u} (must be a vector of length equal to \code{m}), \code{sigsq}, \code{tausq}, \code{p}.  If not specified, initial values are randomly generated.}
  \item{n.sim}{number of MCMC simulations after burn-in and thinning. The total number of MCMC iterations run is \code{n.sim*n.thin + n.burn}. Thus, the final number MCMC simulations in the output of the function will always be \code{n.sim}.}
  \item{n.burn}{number of simulations to burn}
  \item{n.thin}{keep every \code{thin} draw from the MCMC simulations}
  \item{seed}{random number seed for the MCMC simulations}
}

\details{
  Generates MCMC draws from the posterior distribution of the nonparametric Bernstein regression function as described in Curtis and Ghosh (2009).
}

\value{
  An S3 object with class "biso" and the following components:
  \item{mcmctime}{the time it took to run the sampling algorithm. \code{mcmctime} is the output from a call to function \code{system.time}.}
  \item{postdraws}{a data frame with posterior draws of all parameters}
  \item{W}{the "W" matrix}
  \item{x}{original \code{x} values}
  \item{y}{original \code{y} values}
  \item{m}{order of the Bernstein polynomial basis expansion}
  \item{DIC}{"original" DIC value as in Spiegelhalter et. al. (2002)}
  \item{DIC2}{"alternative" DIC value as in Gelman et. al. (2004)}
  \item{pD}{effective number of parameters as in Spiegelhalter et. al. (2002)}
  \item{pD2}{alternative effective number of parameters as in Gelman et. al. (2004)}
}

\references{
  Curtis, S. M. and Ghosh, S. K. (2009) "A variable selection approach to monotonic regression with Bernstein polynomials."

  Gelman, A., et. al. (2003). \emph{Bayesian Data Analysis}. Chapman and Hall / CRC.

  Speigelhalter, D. J., et. al. (2002). "Bayesian measures of model complexity and fit."  JRSSB 64(4): 583--616.
}

\author{
  S. McKay Curtis with many helpful suggestions from Sujit K. Ghosh.
}

\note{
  No further notes.
}

\seealso{\code{\link{pflat.biso}},\code{\link{fitted.biso}},\code{\link{plot.biso}}}

\examples{
\dontrun{
data(childgrowth)

## Run three different chains ##
out1 <- bisoreg(childgrowth$day, childgrowth$height,
                m=80, n.sim=15000, n.thin=10)
out2 <- bisoreg(childgrowth$day, childgrowth$height,
                m=80, n.sim=15000, n.thin=10)
out3 <- bisoreg(childgrowth$day, childgrowth$height,
                m=80, n.sim=15000, n.thin=10)

## Convert to MCMC object ##
out <- list()
out[[1]] <- mcmc(out1$postdraws)
out[[2]] <- mcmc(out2$postdraws)
out[[3]] <- mcmc(out3$postdraws)
mcmcout <- mcmc.list(out)

## Gelman Rubin diagnostics ##
gelman.diag(mcmcout)

## Posterior probability of a flat function ##
pflat(out1)

## Create scatterplot and plot Bayesian regression fit ##
plot(out1, color="black", cl=F, lwd=2, xlab="day", ylab="height")
title("Child Growth Data")

## Compute isoreg fit ##
iout = isoreg(childgrowth$day, childgrowth$height)
lines(as.stepfun(iout), col.h="red", col.v="red", lwd=2)

## Compute monreg estimate of Dette et. al. ##
mout = monreg.wrapper(childgrowth$day, childgrowth$height)
lines(childgrowth$day, mout$est, lwd=2, col="blue")

## Compute local polynomial regression fit ##
lines(childgrowth$day,
      predict(loess.wrapper(childgrowth$day, childgrowth$height)),
      lwd=2, col="green")

## Add a legend ##
legend("topleft",
       legend=c("bayes", "isoreg", "monreg", "loess"),
       col=c("black", "red", "blue", "green"), lwd=2)

## Check sensitivity to the prior on p ##
## p ~ Beta(a=1,b=1) ##
outa1b1 <- bisoreg(childgrowth$day, childgrowth$height,
                   40, priors=list(a.p=1,b.p=1), n.sim=15000, n.thin=10)
## p ~ Beta(a=1,b=3) ##
outa1b3 <- bisoreg(childgrowth$day, childgrowth$height,
                   40, priors=list(a.p=1,b.p=3), n.sim=15000, n.thin=10)
## p ~ Beta(a=3,b=1) ##
outa3b1 <- bisoreg(childgrowth$day, childgrowth$height,
                   40, priors=list(a.p=3, b.p=1), n.sim=15000, n.thin=10)

## Create plot to compare model fits ##
plot(outa1b1,cl=F,color="black")
plot(outa1b3,cl=F,add=T,col="red")
plot(outa3b1,cl=F,add=T,col="green")
gplots:::smartlegend("left","top",
                    legend=c("a=1 b=1","a=1 b=3","a=3 b=1"),
                    col=c("black","red","green"),
                    lwd=1)

## Check sensitivity to the choice of M ##
outm20 <- bisoreg(childgrowth$day, childgrowth$height, m=20, n.sim=15000, n.thin=10)
outm30 <- bisoreg(childgrowth$day, childgrowth$height, m=30, n.sim=15000, n.thin=10)
outm40 <- bisoreg(childgrowth$day, childgrowth$height, m=40, n.sim=15000, n.thin=10)

## Create plot to compare model fits ##
plot(outm20, cl=F, col="blue")
plot(outm30, cl=F, add=T, col="red")
plot(outm40, cl=F, add=T, col="green")
plot(out1, cl=F, add=T, col="black")
gplots:::smartlegend("left", "top",
                    legend=c(20,30,40,80),
                    col=c("blue", "red", "green", "black"),
                    lwd=1)


## A method for choosing 'm' ##
## (from Sujit K. Ghosh) ##

## Piece-wise linear 'true' regression function ##
mu <- function(x){
  ifelse(-3<=x & x<=-1, x, 0) +
      ifelse(-1<x & x<1, -1, 0) +
          ifelse(1<=x & x<3, x - 2, 0)
}
par(mfrow=c(2,2))
curve(mu, -2.9, 2.9)

## Simulate the data ##
n <- 100
x <- runif(n, -2.9, 2.9)
y <- mu(x) + rnorm(n, 0, 0.1)

## Fit initially with default 'm=n/2' ##
fit0 <- bisoreg(x, y, m=floor(n/2))
plot(fit0)
lines(sort(x), mu(sort(x)), col="blue")
fit0$DIC2
fit0$pD2

## Use effective number of parameters ##
## as new value of 'm' ##
fit <- bisoreg(x, y, m=ceiling(fit0$pD2))
plot(fit)
lines(sort(x), mu(sort(x)), col="blue")
fit$DIC2
fit$pD2

## Compare fits ##
plot(fitted(fit0), fitted(fit))
abline(0, 1)
mean(abs(y - fitted(fit0)))
mean(abs(y - fitted(fit)))
}
}
\keyword{smooth}
\keyword{nonparametric}

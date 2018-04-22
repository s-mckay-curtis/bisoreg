`loess.wrapper` <- function(x, y, span.vals=seq(0.25, 1.00, by=0.05), folds=5){
    mae <- numeric(length(span.vals))
    theta.fit <- function(x, y, span) loess(y ~ x, span=span)
    theta.predict <- function(fit, x0) predict(fit, newdata=x0)
    ii=0
    for(span in span.vals){
        ii <- ii + 1
        y.cv <- try(crossval(x, y, theta.fit, theta.predict, span=span, ngroup=folds)$cv.fit, silent=TRUE)
        if (inherits(y.cv, "try-error")){
            mae[ii] <- NA
            warning(paste0("crossval failed at span ", span, ". Likely because span is too small. Span step is ignored."), immediate.=TRUE)
        } else {
            fltr <- !is.na(y.cv)
            mae[ii] <- mean(abs(y[fltr] - y.cv[fltr]))
        }
    }
    span <- span.vals[which.min(mae)]
    out <- loess(y ~ x, span=span)
    return(out)
}

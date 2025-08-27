## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(svFast)

## -----------------------------------------------------------------------------
library(svFast)
# The number of threads used for calculation can be changed with:
#RcppParallel::setThreadOptions(numThreads = 4)

## -----------------------------------------------------------------------------
x <- 1:10
log_(x)
identical(log_(x), log(x))
# log in base 8
log_(x, base = 8)
bench::mark(log(x), log_(x)) # Slower with such a short vector

## -----------------------------------------------------------------------------
x2 <- runif(1e5)
# x2 size larger than 50,000, parallel computation activated
bench::mark(log(x2), log_(x2)) # Faster (depends on number of threads)


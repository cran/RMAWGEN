\name{qqplotWGEN}
\alias{qqplotWGEN}
\title{qqplotWGEN}
\usage{qqplotWGEN(val, xlab="simulated", ylab="measured", main="title",
    ylim=c(min(val), max(val)), xlim=c(min(val), max(val)), diff=FALSE,
    quantile=0)}
\description{Makes a qqplot and Wilcoxon test between the two columns of \code{val}}
\author{Emanuele Cordano, Emanuele Eccel}
\value{Wilcoxon test between the two columnds of 'val'}
\arguments{\item{val}{a matrix with two colomns containing the two samples to be compared}
\item{xlab,ylab,main}{see \code{\link{plot}}}
\item{xlim,ylim}{see \code{\link{plot}}}
\item{diff}{logical variable, if \code{TRUE} the function is applied to \code{diff(val)} instead of \code{val}. See \code{\link{diff}}}
\item{quantile}{quantile value on which data samples in \code{val} are considered. Default is 0.}}

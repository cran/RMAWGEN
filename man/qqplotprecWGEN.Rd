\name{qqplotprecWGEN}
\alias{qqplotprecWGEN}
\title{qqplotprecWGEN}
\usage{qqplotprecWGEN(measured, simulated, xlab="simulated[mm]",
    ylab="measured[mm]", title="daily precipitation", station,
    diff=FALSE, quantile=0)}
\description{Makes a qqplot of measured and simulated data for several stations.}
\note{It uses \code{\link{qqplotWGEN}} and makes a figure for each pair of columns from \code{measured} and \code{simulated}. See the R code for further details.}
\author{Emanuele Cordano, Emanuele Eccel}
\value{0 in case of success}
\arguments{\item{measured}{matrix containing measured data (each station corresponds to a column)}
\item{simulated}{matrix containing respective generated data (each station corresponds to a column)}
\item{xlab,ylab}{see \code{\link{plot}},\code{\link{qqplotWGEN}}}
\item{title}{title}
\item{station}{character vector containing IDs of analyzed station. If \code{NULL} (default) all station (columns of \code{simulated} and \code{measured}) are considered}
\item{diff,quantile}{see \code{\link{qqplotWGEN}}}}

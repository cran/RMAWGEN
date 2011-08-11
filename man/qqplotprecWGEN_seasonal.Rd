\name{qqplotprecWGEN_seasonal}
\alias{qqplotprecWGEN_seasonal}
\title{qqplotprecWGEN_seasonal}
\usage{qqplotprecWGEN_seasonal(measured, simulated, origin="1961-1-1",
    xlab="simulated[mm]", ylab="measured[mm]",
    title="daily_precipitation", directorypdf,
    station=names(simulated))}
\description{Makes four seasonal qqplots  (winter, spring, summer and autumn) of measured and simulated data for several stations.}
\author{Emanuele Cordano, Emanuele Eccel}
\note{Uses \code{\link{qqplotprecWGEN}} for each season of collected data and saves the output on  pdf files. See the R code for further details.}
\seealso{\code{\link{qqplotprecWGEN}},\code{\link{extractmonths}}}
\value{0 in case of success}
\arguments{\item{measured}{matrix containing measured data (each station corresponds to a column)}
\item{simulated}{matrix containing respective generated data (each station corresponds to a column)}
\item{xlab,ylab}{see \code{\link{plot}},\code{\link{qqplotWGEN}}}
\item{title}{title}
\item{station}{character vector containing IDs of analyzed station. If \code{NULL} (default) all station (columns of \code{simulated} and \code{measured}) are considered}
\item{directorypdf}{name of the directory (path included) where to seva the outputs}
\item{origin}{first day of data, see \code{\link{extractmonths}} for format and other information}}

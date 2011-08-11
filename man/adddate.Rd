\name{adddate}
\alias{adddate}
\title{adddate}
\usage{adddate(data, origin="1961-1-1")}
\description{Inserts three columns (year,month,day) passing dates  to a matrix or to a dataframe}
\author{Emanuele Cordano, Emanuele Eccel}
\seealso{\code{\link{findDate}}}
\value{a data frame with dates and \code{data} values}
\arguments{\item{data}{matrix of daily data}
\item{origin}{character string containig the date of the first row of \code{data} as YY-MM-DD}}

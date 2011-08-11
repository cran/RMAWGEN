\name{extractdays}
\alias{extractdays}
\title{extractdays}
\usage{extractdays(data=array(1:ndim_max, dim = c(ndim_max, 1)),
    ndim_max=1e+05, when="1990-1-1", origin="1961-1-1", nday=1)}
\description{Extracts the rows of a matrix corresponding to the requested days (expressed as dates YYYY-MM-DD) given the  date (origin) of the first row}
\author{Emanuele Cordano, Emanuele Eccel}
\note{It uses \code{\link{julian}}}
\value{a matrix containing the requested rows}
\arguments{\item{data}{an input data matrix where each row corresponds to a daily record}
\item{when}{desired dates for which the data are requested}
\item{origin}{date corresponding to the first row of \code{data}}
\item{nday}{(optional) number of days since \code{when} to extract the data}
\item{ndim_max}{maximimum (integer) number of rows in \code{data} where to find \code{when}. 
Default is 100000 and works if \code{data} is missing.}}

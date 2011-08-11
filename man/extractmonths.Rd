\name{extractmonths}
\alias{extractmonths}
\title{extractmonths}
\usage{extractmonths(data=array(1:ndim_max, dim = c(ndim_max, 1)),
    ndim_max=1e+05, when=c("Dec", "Jan", "Feb"), origin="1961-1-1")}
\description{Extracts the rows of a matrix corresponding to requested months of a year given the  date (origin) of the first row}
\author{Emanuele Cordano, Emanuele Eccel}
\value{a matrix containing the requested rows}
\note{It uses \code{\link{months}} and  \code{\link{julian}}}
\seealso{\code{\link{extractdays}}}
\arguments{\item{data}{an input data matrix where each row corresponds to a daily record}
\item{when}{character vactor of months for which the data are required. 
It must be a subset of \code{c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")}}
\item{origin}{date corresponding to the first row of \code{data}}
\item{ndim_max}{maximimum (integer) number of rows in \code{data} where to find \code{when}. 
Default is 100000 and works if \code{data} is missing.}}

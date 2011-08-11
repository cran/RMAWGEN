\name{is.monthly.climate}
\alias{is.monthly.climate}
\title{is.monthly.climate}
\usage{is.monthly.climate(climate, nstation=3, nmonth=12, verbose=TRUE)}
\description{Verifies if 'climate' represents the monthly climatology in one year, i.e  'climate' is monthly.climate type}
\author{Emanuele Cordano, Emanuele Eccel}
\value{A logical variable if the matrix 'climate' is monthly.climate type}
\arguments{\item{climate}{the 'monthly climatology' data}
\item{nstation}{number of variable measurement station (columns of the matrix 'climate')}
\item{nmonth}{number of months in one year (it can be different if climate is represented by seasonal avarages or others), Default is 12}
\item{verbose}{Prints output and warining messagrs only if is \code{TRUE}.}}

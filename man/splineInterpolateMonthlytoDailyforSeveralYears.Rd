\name{splineInterpolateMonthlytoDailyforSeveralYears}
\alias{splineInterpolateMonthlytoDailyforSeveralYears}
\title{splineInterpolateMonthlytoDailyforSeveralYears}
\usage{splineInterpolateMonthlytoDailyforSeveralYears(val, start_year=2010,
    nyear=1, leap=TRUE, offset=2)}
\description{Interpolates monthly data to daily data using \code{\link{splineInterpolateMonthlytoDaily}}  for several years}
\value{a matrix with interpolated daily data}
\seealso{\code{\link{spline}},\code{\link{splineInterpolateMonthlytoDaily}}}
\author{Emanuele Cordano, Emanuele Eccel}
\arguments{\item{val}{matrix containing monthly mean data for one year}
\item{start_year}{first year}
\item{nyear}{number of years since \code{start_year}}
\item{leap}{logical variable If \code{TRUE} (default) leap years are considered, otherwise they are not}
\item{offset}{integer values. Default is 2. Number of years considered beyond the extremes in order to avoid edge errors}}

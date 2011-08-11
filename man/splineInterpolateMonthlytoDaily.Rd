\name{splineInterpolateMonthlytoDaily}
\alias{splineInterpolateMonthlytoDaily}
\title{splineInterpolateMonthlytoDaily}
\usage{splineInterpolateMonthlytoDaily(nday=365, val=as.matrix(cbind(1 *
    (0.5:11.5) * nday/12, 2 * (0.5:11.5) * nday/12)),
    origin="1961-1-1", first_row=1, last_row=nday)}
\description{Interpolates monthly data to daily data using \code{\link{spline}}  and preserving monthly mean values}
\value{a matrix with interpolated daily data}
\seealso{\code{\link{spline}},\code{\link{splineInterpolateMonthlytoDailyforSeveralYears}}}
\author{Emanuele Cordano, Emanuele Eccel}
\arguments{\item{nday}{number of days on which the daily data is requested, e.g. number of days in one year}
\item{val}{matrix containing monthly mean data}
\item{origin}{date corresponding to the first row of the returned matrix}
\item{first_row}{row corresponding the first day of time interval where montlhy mean conservation is applied}
\item{last_row}{corresponding the last day of time interval where montlhy mean conservation is applied}}

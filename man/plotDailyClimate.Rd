\name{plotDailyClimate}
\alias{plotDailyClimate}
\title{plotDailyClimate}
\usage{plotDailyClimate(data, title="Daily_Avereged_Temperture_in_one_year",
    origin="1961-1-1", when="1979-1-1", ylab="Temperature [degC]",
    xlab="Time [days]", nday=365, bicolor=FALSE, col="black", lwd=1)}
\description{Plots daily climatology through one year}
\author{Emanuele Cordano, Emanuele Eccel}
\value{a matrix containing the plotted variables}
\arguments{\item{data}{matrix whose column contains daily-averaged climatic series of variables (e.g. maximum or minumum daily averaged temperature obtained by spline interpolation of monthly climatology)}
\item{origin}{origin date corresponding to the first row of \code{data}}
\item{when}{start day for daily climatology plot}
\item{title,xlab,ylab,col,lwd}{see \code{\link{plot}}}
\item{nday}{number of days in one year. Default is 365.}
\item{bicolor}{logical variable. If \code{TRUE} and \code{data} represents climatologies of minimun and maximum daily temperature, the lines are plotted with blue and red colors respectively.}}

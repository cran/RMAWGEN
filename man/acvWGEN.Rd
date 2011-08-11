\name{acvWGEN}
\alias{acvWGEN}
\title{acvWGEN}
\usage{acvWGEN(measured, simulated, titles=c("Sim.", "Mes."), station)}
\description{Plots an auto-covariance function between measured and simulated data for several stations}
\author{Emanuele Cordano, Emanuele Eccel}
\note{It uses \code{\link{acf}} function}
\value{0 in case of success}
\arguments{\item{measured}{matrix containing measured time series}
\item{simulated}{matrix containing simulated time series}
\item{titles}{title suffixes for the simulated and measured data respectively c("Sim.","Mes.")}
\item{station}{string vector containing the IDs of the meteorological stations where the autocovariance is calculated. 
If it is \code{NULL} (default) all stations (corresponding to the columns of "simulated" and "measured") are applied}}

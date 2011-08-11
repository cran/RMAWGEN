\name{forecastResidual}
\alias{forecastResidual}
\title{forecastResidual}
\usage{forecastResidual(var, xprev, B)}
\description{Forecasts the residual value of a VAR realization given the white noise covariance matrix}
\author{Emanuele Cordano, Emanuele Eccel}
\seealso{\code{\link{forecastEV}},\code{\link{NewVAReventRealization}}}
\value{a vector of values}
\arguments{\item{var}{A VAR model represented by a \code{varest} object as returned by \code{\link{getVARmodel}} or \code{\link{VAR}}}
\item{xprev}{previous status of the random variable, in this case the "current instant"white-noise". Default is \code{NULL} and then randomly generated.}
\item{B}{matrix of coefficients for the vectorial white-noise component}}

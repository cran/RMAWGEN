\name{NewVAReventRealization}
\alias{NewVAReventRealization}
\title{NewVAReventRealization}
\usage{NewVAReventRealization(var, xprev, exogen, B)}
\description{Generates a new realization of a VAR model}
\author{Emanuele Cordano, Emanuele Eccel}
\seealso{\code{\link{forecastEV}},\code{\link{forecastResidual}}}
\value{a vector of values}
\arguments{\item{var}{A VAR model represented by a \code{varest} object as returned by \code{\link{getVARmodel}} or \code{\link{VAR}}}
\item{xprev}{previous status of the random variable}
\item{exogen}{vector containing the values of the "exogen" variables (predictor) for the generation}
\item{B}{matrix of coefficients for the vectorial white-noise component}}

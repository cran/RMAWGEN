\name{newVARmultieventRealization}
\alias{newVARmultieventRealization}
\title{newVARmultieventRealization}
\usage{newVARmultieventRealization(var, xprev=rnorm(var$K * var$p), exogen,
    nrealization=10, B=t(chol(summary(var)$covres)))}
\description{Generates several realizations of a VAR model}
\author{Emanuele Cordano, Emanuele Eccel}
\value{a matrix of values}
\arguments{\item{var}{A VAR model represented by a \code{varest} object as returned by \code{\link{getVARmodel}} or \code{\link{VAR}}}
\item{xprev}{previous status of the random variable}
\item{exogen}{matrix containing the values of the "exogn" variables (predictor) for the generation}
\item{nrealization}{number of realization (e.g. days to simulate). If \code{exogen} is not \code{NULL} and it is a matrix, it must be lower or equal to the number of rows of \code{exogen}}
\item{B}{matrix of coefficients for the vectorial white-noise component}}

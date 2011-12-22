\name{newVARmultieventRealization}
\alias{newVARmultieventRealization}
\title{Generates several realizations of a VAR model}
\usage{
  newVARmultieventRealization(var,
    xprev = rnorm(var@VAR$K * var@VAR$p), exogen = NULL,
    nrealization = 10,
    B = t(chol(summary(var@VAR)$covres)), extremes = TRUE,
    type = 3)
}
\arguments{
  \item{var}{A VAR model represented by a \code{varest2}
  object as returned by \code{\link{getVARmodel}}}

  \item{xprev}{previous status of the random variable}

  \item{exogen}{matrix containing the values of the
  "exogen" variables (predictor) for the generation}

  \item{nrealization}{number of realization (e.g. days to
  simulate). If \code{exogen} is not \code{NULL} and it is
  a matrix, it must be lower or equal to the number of rows
  of \code{exogen}}

  \item{B}{matrix of coefficients for the vectorial
  white-noise component}

  \item{extremes,type}{see \code{\link{inv_GPCA}}}
}
\value{
  a matrix of values
}
\description{
  Generates several realizations of a VAR model
}
\author{
  Emanuele Cordano, Emanuele Eccel
}


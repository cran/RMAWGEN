\name{normalizeGaussian_prec}
\alias{normalizeGaussian_prec}
\title{Converts precipitation values to "Gaussinized" normally-distributed values taking into account the probability of no precipitation occurences. values
or vice versa in case \code{inverse} is \code{TRUE}}
\usage{
  normalizeGaussian_prec(x = 0, data = x, cpf = NULL,
    mean = 0, sd = 1, inverse = FALSE, type = 3,
    extremes = TRUE, sample = NULL, qnull = 0, valmin = 1)
}
\arguments{
  \item{x}{value or vector of values to be converted}

  \item{data}{a sample of data on which a non-parametric
  probility distribution is estimated}

  \item{cpf}{cumulative probability distribution. If
  \code{NULL} (default) is calaculated as
  \code{\link{ecdf}(data)}}

  \item{mean}{mean (expected value) of the normalized
  random variable. Default is 0.}

  \item{sd}{standard deviation of the normalized random
  variable. Default is 1.}

  \item{inverse}{logical value. If \code{TRUE} the function
  works inversely (the opposite way). Default is
  \code{FALSE}.}

  \item{qnull}{probability of no precipitation occurence}

  \item{valmin}{minimum value of precipitation to consider
  a wet day}

  \item{type}{see \code{\link{quantile}}}

  \item{extremes}{logical variable.  If \code{TRUE}
  (default) the probability or frequency is multiplied by
  \deqn{\frac{N}{N+1}} where \eqn{N} is the length of
  \code{data}}

  \item{sample}{a character string or \code{NULL}
  containing sample or probability distribution
  information. Default is \code{NULL}}
}
\value{
  the normalized variable or its inverse
}
\description{
  Converts precipitation values to "Gaussinized"
  normally-distributed values taking into account the
  probability of no precipitation occurences. values or
  vice versa in case \code{inverse} is \code{TRUE}
}
\note{
  This function makes a Marginal Gaussianization accounting
  for the days with zero precipitation. See the R code for
  further details
}
\author{
  Emanuele Cordano, Emanuele Eccel
}
\seealso{
  \code{\link{normalizeGaussian}}
}


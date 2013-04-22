\name{GPCA}
\alias{GPCA}
\title{This function makes a Gaussianization procedure besad on PCA iteration ( see \code{\link{GPCA_iteration}})}
\usage{
  GPCA(x_prev, n = 30, extremes = TRUE)
}
\arguments{
  \item{x_prev}{previous set of the random variable
  \code{x}. If it is a \code{varest} object, the residuals
  are taken into account.}

  \item{n}{number of reiterations}

  \item{extremes}{see
  \code{\link{normalizeGaussian_severalstations}}}
}
\value{
  A \code{\link{GPCA-class}} S3 object returned by
  \code{\link{GPCA_iteration}} at each iteration and the
  final results of the G-PCA procedure (matrix
  \code{final_results})
}
\description{
  This function makes a Gaussianization procedure besad on
  PCA iteration ( see \code{\link{GPCA_iteration}})
}
\note{
  This function re-iterates the equation (1) of "PCA
  Gaussianization for One-Class Remote Sensing Image" by V.
  Laparra et al.,
  \url{http://dx.doi.org/doi/10.1117/12.834011}
}
\author{
  Emanuele Cordano
}
\seealso{
  \code{\link{GPCA-class}} for 'GPCA' S3 class
}


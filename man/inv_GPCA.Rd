\name{inv_GPCA}
\alias{inv_GPCA}
\title{This function makes an inverse Gaussianization procedure besad on PCA iteration ( see \code{\link{inv_GPCA_iteration}}}
\usage{
  inv_GPCA(x = NULL, GPCA_param, type = 3, extremes = TRUE)
}
\arguments{
  \item{x}{gaussian random variable to transform}

  \item{GPCA_param}{\code{\link{GPCA-class}} S3 object
  returned by the function \code{\link{GPCA}}}

  \item{extremes}{see
  \code{\link{normalizeGaussian_severalstations}}}

  \item{type}{see
  \code{\link{normalizeGaussian_severalstations}}}
}
\value{
  the non-Gaussian random variable
}
\description{
  This function makes an inverse Gaussianization procedure
  besad on PCA iteration ( see
  \code{\link{inv_GPCA_iteration}}
}
\note{
  This function re-iterates the inverse of equation (1) of
  "PCA Gaussianization for One-Class Remote Sensing Image"
  by V. Laparra et al.,
  \url{http://dx.doi.org/doi/10.1117/12.834011}
}
\author{
  Emanuele Cordano
}


\name{inv_GPCA_iteration}
\alias{inv_GPCA_iteration}
\title{This function makes an inverse iteration of PCA-Gaussianization process}
\usage{
  inv_GPCA_iteration(x = GPCA_iter_param$x_next,
    GPCA_iter_param, type = 3, extremes = TRUE)
}
\arguments{
  \item{x}{matrix of gaussian random variale to transform}

  \item{GPCA_iter_param}{\code{\link{GPCAiteration}} S3
  object returned by the function
  \code{\link{GPCA_iteration}} corresponding the related
  direct iteration}

  \item{extremes}{see
  \code{\link{normalizeGaussian_severalstations}}}

  \item{type}{see
  \code{\link{normalizeGaussian_severalstations}}}
}
\value{
  the non-Gaussian random variable
}
\description{
  This function makes an inverse iteration of
  PCA-Gaussianization process
}
\note{
  This function is based on the inverse of the equation (1)
  of "PCA Gaussianization for One-Class Remote Sensing
  Image" by V. Laparra et al.,
  \url{http://dx.doi.org/doi/10.1117/12.834011}
}


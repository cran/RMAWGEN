\name{covariance}
\alias{covariance}
\title{covariance}
\usage{covariance(x, data=x, cpf, mean=0, sd=1, step, prec=10^-4,
    use="pairwise.complete.obs", type=3, extremes=TRUE, sample,
    origin_x, origin_data=origin_x)}
\description{Calculates the covariance matrix of the normally standardized variables obtained from the columns of \code{x}}
\author{Emanuele Cordano, Emanuele Eccel}
\value{a matrix with the normalized variable or its inverse}
\seealso{\code{\link{normalizeGaussian_severalstations}},\code{\link{normalizeGaussian}}}
\note{It applies \code{\link{normalizeGaussian_severalstations}} to \code{x} and \code{data} and then calculates the covariances among the column.
See the R code for further details}
\arguments{\item{x}{value to be converted}
\item{data}{a sample of data on which a non-parametric probility distribution is estimated}
\item{cpf}{cumulative probability distribution. If \code{NULL} (default) is calaculated as \code{\link{ecdf}(data)}}
\item{mean}{mean (expected value) of the normalized random variable. Default is 0.}
\item{sd}{standard deviation of the normalized random variable. Default is 1.}
\item{step}{vector of values in which step discontinuities of the cumulative probability function occur. Default is \code{NULL}}
\item{prec}{amplitude of the neighbourhood of the step discontinuities where cumulative probability function is treated as non continuous.}
\item{type}{see \code{\link{quantile}}}
\item{extremes}{logical variable. 
If \code{TRUE} (default) the probability or frequency is multiplied by \deqn{\frac{N}{N+1}} where \eqn{N} is the length of \code{data}}
\item{sample}{information about sample or probability distribution. Default is \code{NULL}}
\item{origin_x}{date corresponding to the first row of \code{x}}
\item{origin_data}{date corresponding to the first row of \code{data}}
\item{use}{see \code{\link{cov}}}}

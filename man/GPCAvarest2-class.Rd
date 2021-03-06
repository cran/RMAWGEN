% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zc2_GPCAvarest2.R
\docType{class}
\name{GPCAvarest2-class}
\alias{GPCAvarest2-class}
\alias{GPCAvarest2}
\title{GPCAvarest2-class}
\description{
This class inherits \code{varest2} and contains all information about GPCA (\code{\link{GPCA}} transformation.
}
\details{
\describe{
    \item{\code{GPCA_data}:}{A \code{"GPCA"} S3 object containing the parameters of the Multi-variate Gaussianization of the time series, it is the result of \code{\link{GPCA}} function applied to the input data of \code{\link{getVARmodel}}

}
    \item{\code{GPCA_residuals}:}{A \code{"GPCA"} S3 object containing the parameters of the Multi-variate Gaussianization of the residuals of the VAR model contained in the \code{VAR} slot; it is \code{NULL} if no Gaussiatization of residuals is applied.
Object of class \code{"list"}  }
    \item{\code{VAR}:}{S3 Object of class \code{"varest"} }
 }

#' @note A \code{GPCAvarest2} object can be created by \code{new("GPCAvarest2", ...)} 
or returned by the function \code{\link{getVARmodel}}
}
\examples{
showClass("GPCAvarest2")

}
\author{
Emanuele Cordano
}
\keyword{classes}

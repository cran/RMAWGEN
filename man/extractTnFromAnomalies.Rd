\name{extractTnFromAnomalies}
\alias{extractTnFromAnomalies}
\title{Extracts generated time series of Daily Minimum Temperature from a random multi-realization obtained by \code{\link{generateTemperatureTimeseries}} function}
\usage{
  extractTnFromAnomalies(res_multigen, std, SplineAdv)
}
\arguments{
  \item{res_multigen}{matrix containing standardized values
  of daily temperature as returned by
  \code{\link{generateTemperatureTimeseries}} (first item)}

  \item{std}{vector containing start deviation for each
  minimun temperature anomalies}

  \item{SplineAdv}{matrix containing the averaged daily
  values of minimun temperature obtained by a spline
  interpolation of the monthly climate}
}
\value{
  a matrix with generated minimum temperature
}
\description{
  Extracts generated time series of Daily Minimum
  Temperature from a random multi-realization obtained by
  \code{\link{generateTemperatureTimeseries}} function
}
\author{
  Emanuele Cordano, Emanuele Eccel
}


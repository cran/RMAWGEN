\name{extractyears}
\alias{extractyears}
\title{extractyears}
\usage{extractyears(data, year_min=1961, year_max=1990, station=c("T0001",
    "T0014", "T0129"))}
\description{Extracts the elements of a data frame corresponding to a period between \code{year_min} and \code{year_max}  for the stations listed in \code{station}}
\author{Emanuele Cordano, Emanuele Eccel}
\value{a matrix containing the requested daily data where each day corresponds to a row and each station corresponds to a column}
\note{The input data frame \code{data} must have the following fields: \code{year,month,day,variables_ID1,variables_ID2,...} 
where the fields \code{,variables_ID1,variables_ID2,...} contain the daily variables referred to the respective stations and the field names are replaced with the respective station ID.}
\arguments{\item{data}{a dataframe containing daily data.}
\item{year_min}{start year}
\item{year_max}{end year}
\item{station}{character vector of the IDs of the station where the data are required}}

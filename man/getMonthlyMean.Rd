\name{getMonthlyMean}
\alias{getMonthlyMean}
\title{Calclates the monthly means of a data frame corresponding to a period between \code{year_min} and \code{year_max}  for stations listed in \code{station}}
\usage{
  getMonthlyMean(data, year_min = 1961, year_max = 1990,
    station = names(data), no_date = FALSE,
    origin = "1961-1-1")
}
\arguments{
  \item{data}{a dataframe containing daily data.}

  \item{year_min}{start year}

  \item{year_max}{end year}

  \item{station}{character vector of the IDs of the station
  where the data are requested}

  \item{no_date}{logical value if \code{TRUE} the function
  \code{extractmonths} is used. Default is \code{FALSE}. It
  is recommended if \code{data} does not contain columns
  for the dates.}

  \item{origin}{date corresponding to the first row}
}
\value{
  a matrix containing the requested monthly mean data where
  each month corresponds to a row and each station
  corresponds to a column
}
\description{
  Calclates the monthly means of a data frame corresponding
  to a period between \code{year_min} and \code{year_max}
  for stations listed in \code{station}
}
\note{
  The input data frame \code{data} must have the following
  fields:
  \code{year,month,day,variables_ID1,variables_ID2,...}
  where the fields \code{,variables_ID1,variables_ID2,...}
  contain the daily variables referred to the respective
  stations and the field names are replaced with the
  respective station ID.
}
\author{
  Emanuele Cordano, Emanuele Eccel
}
\seealso{
  \code{\link{extractyears}}
}


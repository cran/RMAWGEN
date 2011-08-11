\name{PrecipitationEndDay}
\alias{PrecipitationEndDay}
\title{PrecipitationEndDay}
\usage{PrecipitationEndDay(name, station_names, end_day)}
\description{Gets the last day in a precipitation time series, expressed as decimal julian days since 1970-1-1 00:00 UTC}
\author{Emanuele Cordano, Emanuele Eccel}
\value{the precipitation measurement end day given the vectors of station IDs and the precipitation measurement end days}
\arguments{\item{name}{charcacter ID of the station}
\item{station_names}{vector containing the IDs (characters)  of the considered meteorological stations. An example is \code{STATION_NAMES} defined in \code{\link{trentino}}.}
\item{end_day}{vector containing the measurement end day. An example is \code{TEMPERATURE_MEASUREMENT_END_DAY} defined in \code{\link{trentino}}.}}
\examples{data(trentino)
PrecipitationEndDay("T0099",station_names=STATION_NAMES,end_day=PRECIPITATION_MEASUREMENT_END_DAY)}

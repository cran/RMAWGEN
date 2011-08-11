\name{ElevationOf}
\alias{ElevationOf}
\title{ElevationOf}
\usage{ElevationOf(name, station_names, elevation)}
\description{Extracts the elevation of a meteorological station expressed in meters above a reference (sea level)}
\author{Emanuele Cordano, Emanuele Eccel}
\value{the elevation given the vectors of station IDs and the respective elevations}
\arguments{\item{name}{character ID of the station}
\item{station_names}{vector of the IDs (characters) of the considered meteorological stations. An example is \code{STATION_NAMES}, which is defined in the \code{\link{trentino}} dataset.}
\item{elevation}{vector of the elevation  of the considered meteorological stations. An example is \code{ELEVATION}, which is defined in the \code{\link{trentino}} dataset.}}
\examples{data(trentino)
ElevationOf("T0099",station_names=STATION_NAMES,elevation=ELEVATION)}

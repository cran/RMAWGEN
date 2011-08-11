\name{WhereIs}
\alias{WhereIs}
\title{WhereIs}
\usage{WhereIs(name, station_names, location)}
\description{Gets the toponym where a meteorological station is located}
\author{Emanuele Cordano, Emanuele Eccel}
\value{the location toponym given the vectors of station IDs and the respective location toponyms}
\arguments{\item{name}{charcacter ID of the station}
\item{station_names}{vector containing the IDs (characters)  of the considered meteorological stations. An example is \code{STATION_NAMES} defined in the \code{\link{trentino}} dataset.}
\item{location}{vector containing the toponyms. An example is \code{LOCATION} defined in the \code{\link{trentino}} dataset.}}
\examples{data(trentino)
WhereIs("T0099",station_names=STATION_NAMES,location=LOCATION)}

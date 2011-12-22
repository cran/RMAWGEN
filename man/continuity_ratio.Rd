\name{continuity_ratio}
\alias{continuity_ratio}
\title{Calculates the continuity ratio of a set of precipitation measured or generated data in several sites as defined by Wilks, 1998 (see reference link)}
\usage{
  continuity_ratio(data, valmin = 0.5)
}
\arguments{
  \item{data}{containing daily precipitation time series
  for several gauges (one gauge time series per column)}

  \item{valmin}{threshold precipitation value [mm] for
  wet/dry day indicator. If precipitation is lower than
  \code{valmin}, day is considered dry. Default is 0.5 mm.}
}
\value{
  the value of the continuity ratio
}
\description{
  Calculates the continuity ratio of a set of precipitation
  measured or generated data in several sites as defined by
  Wilks, 1998 (see reference link)
}
\author{
  Emanuele Cordano, Emanuele Eccel
}
\references{
  see the following URL references:
  \url{http://onlinelibrary.wiley.com/doi/10.1002/joc.2305/abstract}
  and
  \url{http://www.sciencedirect.com/science/article/pii/S0022169498001863}
}


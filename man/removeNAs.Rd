\name{removeNAs}
\alias{removeNAs}
\title{removeNAs}
\usage{removeNAs(data)}
\description{Replaces each entry of the rows containg NA values with NA}
\author{Emanuele Cordano, Emanuele Eccel}
\seealso{\code{\link{getVARmodel}}}
\value{the matrix \code{data} with the modified rows of NA values}
\note{In \code{\link{getVARmodel}}, 
when using \code{\link{VAR}} or \code{\link{VARselect}}, all NAs will be removed}
\arguments{\item{data}{a matrix}}

\name{getVARmodel}
\alias{getVARmodel}
\title{getVARmodel}
\usage{getVARmodel(data, suffix=c("_Tx", "_Tn"), sep="", p=1, type="none",
    season, exogen, lag.max, ic="AIC", activateVARselect=FALSE,
    na.rm=TRUE)}
\description{Either creates an VAR model or chooses a VAR model by using VAR or VARselect commands of \code{vars} package}
\author{Emanuele Cordano, Emanuele Eccel}
\note{It inherits input parameters of \code{\link{VAR}}, \code{\link{VARselect}} and \code{\link{addsuffixes}}. The variable \code{data} contains the measured data on which the vector auto-regressive models is estimated.
It is a matrix where each row is a realization of the vector random variable. 
In some application of this package, the random variables may be the daily maximum and minimum temperature anomalies for different stations. 
Often the the columns of \code{data} are called with the IDs of the stations whithout specifying the type of variable (e.g. minimun or maximum temperature anomalies). 
This means that two or more columns may have the same name. Therefore the function \code{\link{addsuffixes}}, which is called from this function, adds suitable suffixes to the column names.}
\value{a varest object representing a VAR model}
\arguments{\item{data}{see \code{\link{VAR}} and \code{\link{addsuffixes}}}
\item{suffix}{see  \code{\link{addsuffixes}}}
\item{sep}{separator element for}
\item{p}{lag considered for the auto-regression   see \code{\link{VAR}}}
\item{type}{see \code{\link{VAR}}}
\item{season}{see \code{\link{VAR}}}
\item{exogen}{see \code{\link{VAR}}}
\item{lag.max}{see \code{\link{VARselect}}}
\item{ic}{see \code{\link{VAR}}}
\item{activateVARselect}{logical variables. If \code{TRUE}, the function \code{\link{VARselect}} is run. Default and recommend use is \code{FALSE}.}
\item{na.rm}{logical variables. If \code{TRUE} (default), it takes into account \code{NA} values}}

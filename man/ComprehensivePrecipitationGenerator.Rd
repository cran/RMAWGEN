\name{ComprehensivePrecipitationGenerator}
\alias{ComprehensivePrecipitationGenerator}
\title{The comprehensive Precipitation Generator}
\usage{
  ComprehensivePrecipitationGenerator(station = c("T0001", "T0010", "T0099"),
    prec_all, mean_climate_prec = NULL, year_max = 1990,
    year_min = 1961, leap = TRUE, nmonth = 12, cpf = NULL,
    verbose = TRUE, p = 1, type = "none", lag.max = NULL,
    ic = "AIC", activateVARselect = FALSE, exogen = NULL,
    exogen_sim = NULL, is_exogen_gaussian = FALSE,
    year_max_sim = year_max, year_min_sim = year_min,
    mean_climate_prec_sim = NULL, onlygeneration = FALSE,
    varmodel = NULL, type_quantile = 3, step = 0,
    n_GPCA_iteration = 0,
    n_GPCA_iteration_residuals = n_GPCA_iteration,
    sample = NULL, extremes = TRUE, exogen_all = NULL,
    exogen_all_col = station, option_temp = 0)
}
\arguments{
  \item{station}{character vector of the IDs of the
  considered meteorological stations}

  \item{prec_all}{data frame containing daily precipitation
  of all meteorological stations. See
  \code{\link{PRECIPITATION}} defined in the
  \code{\link{trentino}} dataset for formatting.}

  \item{mean_climate_prec}{a matrix containing monthly mean
  daily precipitation for the considered station. If it is
  \code{NULL}, it is calculated. See input of
  \code{\link{is.monthly.climate}}}

  \item{year_max}{start year of the recorded (calibration)
  period}

  \item{year_min}{end year of the recorded (calibration)
  period}

  \item{leap}{logical variables. If it is \code{TRUE}
  (default), leap years are considered}

  \item{nmonth}{number of months in one year (default is
  12)}

  \item{verbose}{logical variable}

  \item{cpf}{see
  \code{\link{normalizeGaussian_severalstations}}}

  \item{sample,extremes}{see
  \code{\link{normalizeGaussian_severalstations}}}

  \item{step}{see
  \code{\link{normalizeGaussian_severalstations}}. Default
  is 0.}

  \item{p,type,lag.max,ic,activateVARselect}{see respective
  input parameter on \code{\link{getVARmodel}}}

  \item{year_max_sim}{last year of the simulation period.
  Default is equal to \code{year_max}}

  \item{year_min_sim}{fist year of the simulation period.
  Default is equal to \code{year_min}}

  \item{mean_climate_prec_sim}{a matrix containing monthly
  mean daily precipitation for the simulation period. If is
  \code{NULL} (Default), it is set equal to
  \code{mean_climate_prec}.}

  \item{n_GPCA_iteration}{number of iteration of
  Gaussianization process for data. Default is 0 (no
  Gaussianization)}

  \item{n_GPCA_iteration_residuals}{number of iteration of
  Gaussianization process for data. Default is 0 (no
  Gaussianization)}

  \item{exogen}{matrix containing the (normalized or not)
  exogenous variables (predictors) for the recorded
  (calibration) period.}

  \item{exogen_sim}{matrix containing the (normalized or
  not) exogenous variables (predictors) for the simulation
  period. Default is \code{exogen}}

  \item{is_exogen_gaussian}{logical value. If \code{TRUE},
  \code{exogen_sim} and \code{exogen} are given as already
  normalized variables, otherwhise they are not normalized.
  Default is \code{FALSE}}

  \item{onlygeneration}{logical value. If \code{TRUE} the
  VAR model \code{varmodel} is given as input and only
  random generation is done, otherwise (default) is
  calculated from measured data}

  \item{varmodel}{the VAR model as a \code{varest} object.
  If \code{NULL}, it is given as input and only random
  generation is done, otherwise (default) is calculated
  from measured data}

  \item{type_quantile}{see \code{type} on
  \code{\link{quantile}}}

  \item{option_temp}{integer value. If 0 (default),
  exogenous variables (\code{exogen} and \code{exogen_sim}
  ) are not considered, if 1 exogenous variables are
  considered.}

  \item{exogen_all}{data frame containing exogenous
  variable formatted like \code{prec_all}. Default is
  \code{NULL}. It is alternative to \code{exogen} and if it
  not \code{NULL},\code{is_exogen_gaussian} is
  automatically set \code{FALSE}}

  \item{exogen_all_col}{vector of considered columns of
  \code{exogen_all}. Default is \code{station}.}
}
\value{
  A list of the following variables:

  \code{prec_mes} matrix contained measured daily
  precipitation

  \code{prec_spline} matrix containing climatic
  "spline-interpolated" daily preciptation from
  \code{mean_climate_prec}

  \code{data_prec} matrix containing normalized measured
  precipitation variable

  \code{prec_gen} matrix containing generated daily
  precipitation [mm]

  \code{prec_spline_sim} matrix containing climatic
  "spline-interpolated" daily preciptation from
  \code{mean_climate_prec_sim}

  \code{data_prec_gen} matrix containing normalized
  generated precipitation variable

  \code{mean_climate_prec} matrix containing monthly mean
  daily precipitation (historical scenario)

  \code{mean_climate_prec_sim} matrix containing monthly
  mean daily precipitation (predicted/simulated scenario)

  \code{var} a varest object containing the used VAR model
}
\description{
  The comprehensive Precipitation Generator
}
\note{
  It pre-processes and generates a multi-site precipitation
  fields. It uses \code{\link{getVARmodel}}. Unfortunately,
  using this approach, the spatial correlations are
  underestimated. This is due to the persinstence of zeros
  in the precipitation records. This problem is known in
  literature and can be solved in the future versions of
  RMAWGEN. See the R code for further details
}
\examples{
data(trentino)


	vstation <- c("B2440","B6130","B8570","B9100","LAVIO","POLSA","SMICH","T0001",
 "T0010","T0014","T0018","T0032","T0064","T0083","T0090","T0092","T0094","T0099",
 "T0102","T0110","T0129","T0139","T0147","T0149","T0152","T0157","T0168","T0179","T0189","T0193","T0204","T0210","T0211","T0327","T0367","T0373")
	generation_prec <- ComprehensivePrecipitationGenerator(station=vstation[10:12],prec_all=PRECIPITATION,sample="monthly")
	str(generation_prec)
	print(generation_prec$var)
}
\author{
  Emanuele Cordano, Emanuele Eccel
}
\seealso{
  \code{\link{splineInterpolateMonthlytoDailyforSeveralYears}}
}


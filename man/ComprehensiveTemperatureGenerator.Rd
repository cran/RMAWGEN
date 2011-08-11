\name{ComprehensiveTemperatureGenerator}
\alias{ComprehensiveTemperatureGenerator}
\title{ComprehensiveTemperatureGenerator}
\usage{ComprehensiveTemperatureGenerator(station=c("T0001", "T0010", "T0099"),
    Tx_all, Tn_all, mean_climate_Tn, mean_climate_Tx, year_max=1990,
    year_min=1961, leap=TRUE, nmonth=12, verbose=TRUE, p=1,
    type="none", lag.max, ic="AIC", activateVARselect=FALSE,
    year_max_sim=year_max, year_min_sim=year_min, mean_climate_Tn_sim,
    mean_climate_Tx_sim, onlygeneration=FALSE, varmodel,
    normalize=TRUE, type_quantile=3, sample, option=2, exogen,
    exogen_sim=exogen, is_exogen_gaussian=FALSE, exogen_all,
    exogen_all_col=station)}
\description{The comprehensive Temperature Generator}
\author{Emanuele Cordano, Emanuele Eccel}
\seealso{\code{\link{setComprehensiveTemperatureGeneratorParameters}}, \code{\link{generateTemperatureTimeseries}} ,\code{\link{generateTemperatureTimeseries}}.}
\note{It pre-processes series and generates multi-site temperature fields by using \code{\link{setComprehensiveTemperatureGeneratorParameters}},\code{\link{getVARmodel}} and \code{\link{generateTemperatureTimeseries}}.}
\value{A list of the following variables: 

\code{input}   list of variables returned by  \code{\link{setComprehensiveTemperatureGeneratorParameters}}

\code{var}     varest object containing the used VAR model (if useVAR is true), NULL (otherwise)

\code{output}  list variables returned by  \code{\link{generateTemperatureTimeseries}} (i.e. generated timeseries)}
\arguments{\item{station}{see respective input parameter on \code{\link{setComprehensiveTemperatureGeneratorParameters}}}
\item{Tx_all,Tn_all,mean_climate_Tn,mean_climate_Tx}{see respective input parameter on \code{\link{setComprehensiveTemperatureGeneratorParameters}}}
\item{year_max,year_min,leap,nmonth,verbose}{see respective input parameter on \code{\link{setComprehensiveTemperatureGeneratorParameters}}}
\item{p,type,lag.max,ic,activateVARselect}{see respective input parameter on \code{\link{getVARmodel}}}
\item{year_max_sim}{last year of the simulation period. Default is equal to \code{year_max}}
\item{year_min_sim}{fist year of the simulation period. Default is equal to \code{year_min}}
\item{mean_climate_Tn_sim}{monthly avaraged daily minimum temperatures for the simulated scenario and used by the random generator .  Default is \code{mean_climate_Tn}}
\item{mean_climate_Tx_sim}{monthly avaraged daily maximum temperatures for the simulated scenario and used by the random generator .  Default is \code{mean_climate_Tx}}
\item{onlygeneration}{logical variable. If \code{TRUE} the VAR model \code{varmodel} is given as input and only random generation is done, otherwise (default) is calculated from measured data}
\item{varmodel}{the VAR model as a \code{varest} object. If \code{NULL}, it is  given as input and only random generation is done, otherwise (default) is calculated from measured data}
\item{normalize,sample}{see \code{\link{normalizeGaussian_severalstations}} or \code{\link{setComprehensiveTemperatureGeneratorParameters}}}
\item{type_quantile}{see \code{type} on \code{\link{quantile}}}
\item{option}{integer value. If 1, the generator works with minimun and maximum temperature, if 2 (default) it works with the average value between maximum and minimum temparature and the respective daily thermal range.}
\item{exogen}{matrix containing the (normalized or not) exogenous variables (predictors) for the recorded (calibration) period. Default is NULL}
\item{exogen_sim}{matrix containing the (normalized or not) exogenous variables (predictors) for the simulation period. Default is \code{exogen}}
\item{is_exogen_gaussian}{logical value, If \code{TRUE}, \code{exogen_sim} and \code{exogen} are given as already normalized variables, otherwhise they are not normalized. Default is \code{FALSE}}
\item{exogen_all}{data frame containing exogenous variable formatted like \code{Tx_all} and {Tn_all}. Default is \code{NULL}. 
It is alternative to \code{exogen} and if it not \code{NULL},\code{is_exogen_gaussian} is automatically set \code{FALSE}}
\item{exogen_all_col}{vector of considered  columns of \code{exogen_all}. Default is \code{station}.}}
\examples{data(trentino)
set.seed(1233)
vstation <- c("B2440","B6130","B8570","B9100","LAVIO","POLSA","SMICH","T0001",
"T0010","T0014","T0018","T0032","T0064","T0083","T0090","T0092","T0094","T0099",
"T0102","T0110","T0129","T0139","T0147","T0149","T0152","T0157","T0168","T0179","T0189","T0193","T0204","T0210","T0211","T0327","T0367","T0373")		

generation0 <- ComprehensiveTemperatureGenerator(station=vstation[5:8],Tx_all=TEMPERATURE_MAX,Tn_all=TEMPERATURE_MIN,year_min=1961,year_max=1990)
VAR_model <- generation0$var
CX <- generation0$input$monthly_mean_Tx
CN <- generation0$input$monthly_mean_Tn
generation1 <- ComprehensiveTemperatureGenerator(station=vstation[5:8],varmodel=generation0$var,
onlygeneration=TRUE,year_min=1961,year_max=1990,mean_climate_Tn_sim=CN,mean_climate_Tx_sim=CX,year_min_sim=1961,year_max_sim=1990,
Tx_all=TEMPERATURE_MAX,Tn_all=TEMPERATURE_MIN)

str(generation0)
str(generation1)}

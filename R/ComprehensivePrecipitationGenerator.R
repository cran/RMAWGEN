NULL
#'
#' The comprehensive Precipitation Generator
#' 

#' @param station character vector of the IDs of the considered meteorological stations
#' @param prec_all data frame containing daily precipitation of all meteorological stations. See \code{\link{PRECIPITATION}} defined in the \code{\link{trentino}} dataset for formatting.
#' @param mean_climate_prec  a matrix containing monthly mean daily precipitation for the considered station. If it is \code{NULL}, it is calculated. See input of \code{\link{is.monthly.climate}}
#' @param year_max start year of the recorded (calibration) period 
#' @param year_min end year of the recorded (calibration) period
#' @param leap logical variables. If it is \code{TRUE} (default), leap years are considered
#' @param nmonth number of months in one year (default is 12)
#' @param verbose logical variable
#' @param cpf see \code{\link{normalizeGaussian_severalstations_prec}}
#' @param sample,extremes,qnull,valmin see \code{\link{normalizeGaussian_severalstations_prec}}
#' @param step see \code{\link{normalizeGaussian_severalstations_prec}}. Default is 0.
#' @param p,type,lag.max,ic,activateVARselect see respective input parameter on \code{\link{getVARmodel}}
#' @param year_max_sim last year of the simulation period. Default is equal to \code{year_max} 
#' @param year_min_sim fist year of the simulation period. Default is equal to \code{year_min}
#' @param mean_climate_prec_sim a matrix containing monthly mean daily precipitation for the simulation period. If is \code{NULL} (Default), it is set equal to \code{mean_climate_prec}. 
#' @param n_GPCA_iteration number of iteration of Gaussianization process for data. Default is 0 (no Gaussianization) 
#' @param n_GPCA_iteration_residuals number of iteration of Gaussianization process for data. Default is 0 (no Gaussianization)
#' @param exogen matrix containing the (normalized or not) exogenous variables (predictors) for the recorded (calibration) period. 
#' @param exogen_sim  matrix containing the (normalized or not) exogenous variables (predictors) for the simulation period. Default is \code{exogen}
#' @param is_exogen_gaussian logical value. If \code{TRUE}, \code{exogen_sim} and \code{exogen} are given as already normalized variables, otherwhise they are not normalized. Default is \code{FALSE}
#' @param onlygeneration logical value. If \code{TRUE} the VAR model \code{varmodel} is given as input and only random generation is done, otherwise (default) is calculated from measured data 
#' @param varmodel the VAR model as a \code{varest} object. If \code{NULL}, it is  given as input and only random generation is done, otherwise (default) is calculated from measured data 
#' @param type_quantile see \code{type} on \code{\link{quantile}}
#' @param option_temp integer value. If 0 (default), exogenous variables (\code{exogen} and \code{exogen_sim} ) are not considered, if 1 exogenous variables are considered.  
#' @param exogen_all data frame containing exogenous variable formatted like \code{prec_all}. Default is \code{NULL}. 
#' It is alternative to \code{exogen} and if it not \code{NULL},\code{is_exogen_gaussian} is automatically set \code{FALSE}	
#' @param exogen_all_col vector of considered  columns of \code{exogen_all}. Default is \code{station}.
#' @param no_spline logical value. See \code{\link{splineInterpolateMonthlytoDailyforSeveralYears}}. Default is \code{TRUE}.
#' @param nscenario number of possible generated scenarios for daily maximum and minimum temperature
#' @param seed seed for stochastic random generation see \code{\link{set.seed}}
#'   
#' 
#' 
#' 
#' @export 


#' 
#' 
#' @author  Emanuele Cordano, Emanuele Eccel
#'    
#' @seealso \code{\link{splineInterpolateMonthlytoDailyforSeveralYears}} 
#' 
#'        
#'
#' @note It pre-processes and generates a multi-site precipitation fields. It uses \code{\link{getVARmodel}}. Detailed examples can be viewed of this function in \href{https://docs.google.com/file/d/0B8xDtMCnW3dJU2JIemVqMnpKTHc/edit}{this presentation}.
#' Unfortunately, using this approach, the spatial correlations are underestimated. This is due to the persinstence of zeros in the precipitation records.
#' This problem is known in literature and can be solved in the future versions of RMAWGEN.
#' See the R code for further details
#'   
#' 
#' 
#' 

#' @return  A list of the following variables: 
#' 
#' 
#' \code{prec_mes}        matrix contained measured daily precipitation
#' 
#' \code{prec_spline}      matrix containing climatic "spline-interpolated" daily preciptation from \code{mean_climate_prec}
#' 
#' \code{data_prec}        matrix containing normalized measured precipitation variable
#' 
#' \code{prec_gen}          matrix containing generated daily precipitation [mm]
#' 
#' \code{prec_spline_sim}   matrix containing climatic "spline-interpolated" daily preciptation from \code{mean_climate_prec_sim}
#' 
#' \code{data_prec_gen}            matrix containing normalized generated precipitation variable
#' 
#' \code{mean_climate_prec}        matrix containing monthly mean daily precipitation (historical scenario)
#' 
#' \code{mean_climate_prec_sim}    matrix containing monthly mean daily precipitation (predicted/simulated scenario)
#' 
#' \code{var}                 a varest object containing the used VAR model
#' 

#'  
#' 
#' 

#' @examples data(trentino)
#'  
#' 
#' rm(list=ls())
#' set.seed(1222)
#' library(RMAWGEN)
#' data(trentino)

#'year_max <- 1990
#'year_min <- 1961
#'year_max_sim <- 1982
#'year_min_sim <- 1981
#origin <- "1961-1-1"
#'
#'n_GPCA_iter <- 2
#' p <- 1
#'nscenario=1
#'station <- c("T0090","T0083") #,"T0099","T0001") 
#
#'generation00 <- ComprehensivePrecipitationGenerator(station=station,prec_all=PRECIPITATION,year_min=year_min,year_max=year_max,year_min_sim=year_min_sim,year_max_sim=year_max_sim,p=p,n_GPCA_iteration=n_GPCA_iter,n_GPCA_iteration_residuals=0,sample="monthly",nscenario=nscenario,no_spline=FALSE)

## 
## #' 
## #' 
## #' 
## #' year_max <- 1990
## #'  year_min <- 1961
## #' 
## #' year_min_sim <- 1982
## #' year_max_sim <- 1983
## #' 
## # origin <- "1961-1-1"
## #' n_GPCA_iter <- 10
## #' n_GPCA_iteration_residuals <- 10
## #' 	vstation <- c("B2440","B6130","B8570","B9100","LAVIO","POLSA","SMICH","T0001",
## #'  "T0010","T0014","T0018","T0032","T0064","T0083","T0090","T0092","T0094","T0099",
## #'  "T0102","T0110","T0129","T0139","T0147","T0149","T0152","T0157","T0168","T0179","T0189","T0193","T0204","T0210","T0211","T0327","T0367","T0373")		
## #' 	generation00 <-ComprehensiveTemperatureGenerator(station=vstation[14:15],Tx_all=TEMPERATURE_MAX,Tn_all=TEMPERATURE_MIN,year_min=year_min,year_max=year_max,p=1,n_GPCA_iteration=n_GPCA_iter,n_GPCA_iteration_residuals=n_GPCA_iteration_residuals,sample="monthly",year_min_sim=year_min_sim,year_max_sim=year_max_sim)

## # 	str(generation00)
## #	print(generation00$var)
## #' 
## #' 
## #' 
## #' 
## #' 
## #' 
## #' 
## #' 

ComprehensivePrecipitationGenerator <-
function(
		station=c("T0001","T0010","T0099"),
		
		prec_all,
	
		mean_climate_prec=NULL,
		year_max=1990,
		year_min=1961,
		leap=TRUE,
		nmonth=12,
		cpf=NULL,
		verbose=TRUE,
		p=1,
		type="none",
		lag.max=NULL,
		ic="AIC",
		activateVARselect=FALSE,
		exogen=NULL,
		exogen_sim=NULL,
		is_exogen_gaussian=FALSE,
		year_max_sim=year_max,
		year_min_sim=year_min,
		mean_climate_prec_sim=NULL,
		onlygeneration=FALSE,
		varmodel=NULL,
		type_quantile=3,
		qnull=NULL,
		valmin=0.5,
		step=0,
		n_GPCA_iteration=0,
		n_GPCA_iteration_residuals=n_GPCA_iteration,
		
		sample=NULL,
		extremes=TRUE,
		exogen_all=NULL,
		exogen_all_col=station,
		
		
		option_temp=0,
		no_spline=FALSE,
		nscenario=1,
		seed=NULL

) {
	

	
	useVAR=TRUE	
	origin <- paste(year_min,"1","1",sep="/") # Must start from Jan 1 
	origin_sim <- paste(year_min_sim,"1","1",sep="/") # Must start from Jan 1 
	
	
	prec_mes <- as.data.frame(extractyears(prec_all,year_min=year_min,year_max=year_max,station=station))
	
	nyear <- year_max-year_min+1
	if (!is.monthly.climate(mean_climate_prec,nstation=length(station),nmonth=nmonth,verbose=verbose)) mean_climate_prec <- getMonthlyMean(prec_all,year_min=year_min,year_max=year_max,station=station)
	MEAN_CLIMATE_prec_SAVED <- mean_climate_prec
	# TO DO SOME MODIFICATIONS
	prec_spline <- as.data.frame(splineInterpolateMonthlytoDailyforSeveralYears(val=mean_climate_prec,start_year=year_min,nyear=nyear,leap=leap,no_spline=no_spline))	
	names(prec_spline) <- names(mean_climate_prec)
	
	if (min(prec_spline)<=0) {
		
		print("Error in Precipitation generator: spline interpolation of daily precipitation less than 0!")
		return(-1)
	}
	
	data_prec <- normalizeGaussian_severalstations(x=prec_mes,data=prec_mes,sample=sample,cpf=cpf,step=step,origin_x=origin,origin_data=origin,extremes=extremes)
#	data_prec <- normalizeGaussian_severalstations_prec(x=prec_mes,data=prec_mes,sample=sample,cpf=cpf,qnull=qnull,valmin=valmin,origin_x=origin,origin_data=origin,extremes=extremes)
	if (!onlygeneration){

		if (!is.null(exogen_all)) {
			
			exogen <- as.data.frame(extractyears(exogen_all,year_min=year_min,year_max=year_max,station=exogen_all_col))
			is_exogen_gaussian=FALSE
			
			
		}
		if (is.null(exogen_sim)) exogen_sim <- exogen
		
		if (!is_exogen_gaussian){
						
			exogen0 <- exogen
			if (!is.null(exogen)) exogen <- normalizeGaussian_severalstations(x=exogen0,data=exogen0,sample=sample,cpf=cpf,origin_x=origin,origin_data=origin)
		}
						
	
		var <- getVARmodel(data=data_prec,suffix=NULL,sep="",p=p,type=type,exogen=exogen,lag.max=lag.max,ic=ic,activateVARselect=activateVARselect,n_GPCA_iteration_residuals=n_GPCA_iteration_residuals,n_GPCA_iteration=n_GPCA_iteration,extremes=extremes) 
	
		if (activateVARselect) return(var)
			
	} else {
		var <- varmodel		
	}
				
				
				
	if (is.null(mean_climate_prec_sim)) mean_climate_prec_sim <- mean_climate_prec
	nyear_sim <- year_max_sim-year_min_sim+1
	nyear_max <- max(nyear_sim,nyear)
##	print(nyear_sim)
##	print(nyear_max)
	prec_spline_sim <- as.data.frame(splineInterpolateMonthlytoDailyforSeveralYears(val=mean_climate_prec_sim,start_year=year_min_sim,nyear=nyear_max,leap=leap,no_spline=no_spline))	
	prec_spline_sim2 <- as.data.frame(splineInterpolateMonthlytoDailyforSeveralYears(val=mean_climate_prec_sim,start_year=year_min_sim,nyear=nyear_sim,leap=leap,no_spline=no_spline))# This is a spline interpotation only for the years on which generatiion is requested
	names(prec_spline_sim) <- colnames(mean_climate_prec_sim)	
	names(prec_spline_sim2) <- colnames(mean_climate_prec_sim)	
	
	if (is.null(exogen_sim)) exogen_sim <- exogen 
	if (!is.null(exogen_sim) & (!is_exogen_gaussian)) {
		
		exogen_sim0 <- exogen_sim
		exogen_sim <- normalizeGaussian_severalstations(x=exogen_sim0,data=exogen_sim0,sample=sample,cpf=cpf,origin_x=origin_sim,origin_data=origin_sim,extremes=extremes) 
		
	}

	if (!is.null(seed))	set.seed(seed)
	
	data_prec_gen <- newVARmultieventRealization(var,exogen=exogen_sim,nrealization=nrow(prec_spline_sim2),extremes=extremes,type=type_quantile) 
#	data_prec_gen <- newVARmultieventRealization(var,exogen=exogen_sim,nrealization=nyear_sim,extremes=extremes,type=type_quantile) 
	precrows <- 1:(min(c(nrow(prec_mes),nrow(prec_spline),nrow(prec_spline_sim))))
	
	prec_mes_rescaled <- prec_mes[precrows,]/prec_spline[precrows,]*prec_spline_sim[precrows,]	
		
	prec_gen <- as.data.frame(normalizeGaussian_severalstations(x=data_prec_gen,data=prec_mes_rescaled,inverse=TRUE,type=type_quantile,step=step,sample=sample,origin_x=origin_sim,origin_data=origin,extremes=extremes))
#	data_prec <- normalizeGaussian_severalstations_prec(x=prec_mes,data=prec_mes,sample=sample,cpf=cpf,qnull=qnull,valmin=valmin,origin_x=origin,origin_data=origin,extremes=extremes)
	names(prec_gen) <- names(prec_spline_sim)	
	colnames(data_prec_gen) <- names(prec_spline_sim)	# added on may 4 2012
  #  names(prec_gen) <- names(prec_mes)
	out <- NULL
	if (onlygeneration) {
		names_out <- c("prec_gen","prec_spline_sim","data_prec_gen","mean_climate_prec_sim","prec_mes","prec_spline","prec_mes_rescaled")
		for (it in names_out) { if (!exists(it)) assign(it,NULL)}
		out <- list(prec_gen,prec_spline_sim,data_prec_gen,mean_climate_prec_sim,prec_mes,prec_spline,prec_mes_rescaled)# May 2012
#		out <- list(prec_gen,prec_spline_sim,data_prec,prec_gen,prec_spline_sim,data_prec_gen,mean_climate_prec,mean_climate_prec_sim,var)
		names(out) <- names_out
	} else {
		names_out <- c("prec_mes","prec_spline","data_prec","prec_gen","prec_spline_sim","data_prec_gen","mean_climate_prec","mean_climate_prec_sim","var")
		for (it in names_out) { if (!exists(it)) assign(it,NULL)}
		out <- list(prec_mes,prec_spline,data_prec,prec_gen,prec_spline_sim,data_prec_gen,mean_climate_prec,mean_climate_prec_sim,var)
		names(out) <- names_out
		
		
	}

	
	if (nscenario>1) {
		
		for (kk in 1:nscenario) {
		
			# DA METTERE A POSTO!!
    		data_prec_gen <- newVARmultieventRealization(var,exogen=exogen_sim,nrealization=nrow(prec_spline_sim),extremes=extremes,type=type_quantile) 
			colnames(data_prec_gen) <- names(prec_spline_sim)
	#		ntn_rows <- 1:nrow()
			
	#		prec_mes_rescaled <- prec_mes/prec_spline*prec_spline_sim	
	
			precrows <- 1:(min(c(nrow(prec_mes),nrow(prec_spline),nrow(prec_spline_sim))))
			
			prec_mes_rescaled <- prec_mes[precrows,]/prec_spline[precrows,]*prec_spline_sim[precrows,]	
			
			prec_gen <- as.data.frame(normalizeGaussian_severalstations(x=data_prec_gen,data=prec_mes_rescaled,inverse=TRUE,type=type_quantile,step=step,sample=sample,origin_x=origin_sim,origin_data=origin,extremes=extremes))
	#		prec_gen <- as.data.frame(normalizeGaussian_severalstations_prec(x=data_prec_gen,data=prec_mes_rescaled,inverse=TRUE,type=type_quantile,qnull=qnull,valmin=valmin,sample=sample,origin_x=origin_sim,origin_data=origin,extremes=extremes))
			names(prec_gen) <- names(prec_spline_sim)	
			
			prec_index <- sprintf("prec_gen%05d",kk)
			
			out[[prec_index]] <- prec_gen
	
		}	
		
	}
	
	
	
	
	return(out)
#	return(list(prec_mes=prec_mes,prec_spline=prec_spline,data_prec=data_prec,prec_gen=prec_gen,prec_spline_sim=prec_spline_sim,data_prec_gen=data_prec_gen,mean_climate_prec=mean_climate_prec,mean_climate_prec_sim=mean_climate_prec_sim,var=var))
	
}


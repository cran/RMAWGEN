NULL


#' 
#' Computes climatic and correlation information useful for creating an auto-regeressive random generation of maximum and minimun daily temparature 
#' 
#' 
#' @author Emanuele Cordano, Emanuele Eccel
#'    
#' @param station character vector of the IDs of the considered meteorological stations
#' @param Tx_all data frame containing daily maximum temperature of all meteorological station. See \code{\link{TEMPERATURE_MAX}} for formatting.
#' @param Tn_all data frame containing daily minimum temperature of all meteorological station. See \code{\link{TEMPERATURE_MIN}} for formatting.
#' @param mean_climate_Tn a matrix containing monthly mean minimum daily temperature for the considered station. If \code{NULL}, it is calculated. See input of \code{\link{is.monthly.climate}}
#' @param mean_climate_Tx a matrix containing monthly mean maximum daily temperature for the considered station. If \code{NULL}, it is calculated. See input of \code{\link{is.monthly.climate}}
#' @param year_max start year of the recorded (calibration) period 
#' @param year_min end year of the recorded (calibration) period
#' @param leap logical variables. It is \code{TRUE} (Default) if leap years are considered
#' @param nmonth number of months in one year, 12 is Default
#' @param verbose logical variable
#' @param cpf see \code{\link{normalizeGaussian_severalstations}}
#' @param normalize logical variable If \code{TRUE} \code{\link{normalizeGaussian_severalstations}} is used, otherwise it is not. If \code{option} is 2, it is always \code{TRUE}.
#' @param sample see \code{\link{normalizeGaussian_severalstations}}
#' @param option integer value. If 1, the generator works with minimun and maximum temperature, if 2 (default) it works with the average value between maximum and minimum temparature and the respective daily thermal range.
#'
#'   
#' 
#' @export   
#' @return This function creates and returns the following gloabal variables: 
#' 
#' 
#' 
#' 
#' \code{data_original} matrix containing normalized and standardized data (i.e. \code{data_original})
#' 
#' \code{data_for_var}  matrix returned from \code{normalizeGaussian_severalstations} by processing \code{data_original} if \code{normalize} is \code{TRUE}), otherwise it is equal to \code{data_original}. 
#'  
#' \code{Tn_mes} matrix containing measured minimum daily temperature in the analyzed time period ( \eqn{Tn_{mes}})
#' 
#' \code{Tx_mes} matrix containing measured maximum daily temperature in the analyzed time period ( \eqn{Tx_{mes}})
#' 
#' \code{Tm_mes} matrix calculated as to \deqn{\frac{Tx_{mes}+Tn_{mes}}{2}}
#'
#' \code{DeltaT_mes} matrix corresponding to \eqn{Tx_{mes}-Tn_{mes}}
#' 
#' \code{monthly_mean_Tn} matrix containing monthly mean minimum daily temperature for the considered station. It is calculated according to the input format \code{\link{is.monthly.climate}} if \code{saveMonthlyClimate} is  \code{TRUE}. 
#'
#' \code{monthly_mean_Tx} matrix containing monthly mean maximum daily temperature for the considered station. It is calculated according to the input format \code{\link{is.monthly.climate}} if \code{saveMonthlyClimate} is  \code{TRUE}. 
#'
#' \code{SplineAdvTx} matrix containing the averaged  daily values of maximimun temperature obtained by a spline interpolation of the monthly climate \code{monthly_mean_Tx} or \code{mean_climate_Tx} 
#' using \code{\link{splineInterpolateMonthlytoDailyforSeveralYears}} ( \eqn{Tx_{s}}) 
#'
#' \code{SplineAdvTn} matrix containing the averaged  daily values of minimun temperature obtained by a spline interpolation of the monthly climate \code{monthly_mean_Tn} or \code{mean_climate_Tn}  
#' using \code{\link{splineInterpolateMonthlytoDailyforSeveralYears}} ( \eqn{Tn_{s}})
#' 
#' \code{SplineAdvTm} matrix calculated as \eqn{\frac{Tx_{s}+Tn_{s}}{2}}
#'
#' \code{SplineAdvDeltaT}, matrix corresponding to \eqn{Tx_{s}-Tn_{s}}
#'
#' \code{stdTn} vector containing the standard deviation of minimum temperature anomalies \eqn{Tn_{mes}-Tn_s} (\eqn{\sigma_{Tn}})
#'
#' \code{stdTx} vector containing the standard deviation of maximum temperature anomalies \eqn{Tx_{mes}-Tx_s} (\eqn{\sigma_{Tx}})
#'
#' \code{stdTm} vector containing the standard deviation of "mean" temperature anomalies \eqn{Tm_{mes}-Tm_s} (\eqn{\sigma_{Tm}})
#'
#' \code{Tn_mes_res} standard core (standardization) of \eqn{Tn_mes} obtained 
#' by solving column by column the expression  \deqn{\frac{Tn_{mes}-Tn_s}{\sigma_{Tn}}}
#'
#' \code{Tx_mes_res} standard core (standardization) of \eqn{Tx_mes} obtained 
#' by solving column-by-column the expression  \deqn{\frac{Tx_{mes}-Tn_s}{sd_{Tm}}}
#'
#' \code{Tm_mes_res} standard core (standardization) of \eqn{Tm_mes} obtained 
#' by solving column-by-column the expression  \deqn{\frac{Tm_{mes}-Tn_s}{sd_{Tm}}}
#'
#' \code{DeltaT_mes_res} equal to \code{DeltaT_mes}
#'
#' \code{data_original} matrix obtaied as \code{cbind(Tx_mes_res,Tn_mes_res)} if \code{option}==1, or \code{cbind(Tm_mes_res,DeltaT_mes_res)} if \code{option}==2
#' 
#' See the R code for further details.
#'   

#Value 0 (integer) in case of success, -1 otherwise



setComprehensiveTemperatureGeneratorParameters <-
function(station,
		Tx_all,
		Tn_all,
		mean_climate_Tn=NULL,
		mean_climate_Tx=NULL,
		year_max=1990,
		year_min=1961,
		leap=TRUE,
		nmonth=12,
		verbose=FALSE,
		cpf=NULL,
		normalize=TRUE,
		sample=NULL,
		option=2
		
) {

	station <- station[(station %in% names(Tx_all)) & (station %in% names(Tn_all))]
	
	origin <- paste(year_min,"1","1",sep="-") # All cutoff timeseries starts from January 1st of the year_min 
	
	
	Tn_mes <- as.data.frame(extractyears(Tn_all,year_min=year_min,year_max=year_max,station=station))
	Tx_mes <- as.data.frame(extractyears(Tx_all,year_min=year_min,year_max=year_max,station=station))
	Tm_mes <- (Tn_mes+Tx_mes)/2.0
	DeltaT_mes <- Tx_mes-Tn_mes
	
	if (!is.monthly.climate(mean_climate_Tx,nstation=length(station),nmonth=nmonth,verbose=verbose)) mean_climate_Tx <- getMonthlyMean(as.data.frame(Tx_mes),year_min=year_min,year_max=year_max,station=station,no_date=TRUE,origin=origin)
	if (!is.monthly.climate(mean_climate_Tn,nstation=length(station),nmonth=nmonth,verbose=verbose)) mean_climate_Tn <- getMonthlyMean(as.data.frame(Tn_mes),year_min=year_min,year_max=year_max,station=station,no_date=TRUE,origin=origin)
	

		
	monthly_mean_Tx <- mean_climate_Tx
	monthly_mean_Tn <- mean_climate_Tn
		

	
	nyear <- year_max-year_min+1
	
	
	SplineAdvTx <- as.data.frame(splineInterpolateMonthlytoDailyforSeveralYears(val=mean_climate_Tx,start_year=year_min,nyear=nyear,leap=leap))
	SplineAdvTn <- as.data.frame(splineInterpolateMonthlytoDailyforSeveralYears(val=mean_climate_Tn,start_year=year_min,nyear=nyear,leap=leap))
	
	
	
	names(SplineAdvTx) <- names(mean_climate_Tx)
	names(SplineAdvTn) <- names(mean_climate_Tn) 
	
	SplineAdvTm <- (SplineAdvTx+SplineAdvTn)/2.0
	SplineAdvDeltaT <- (SplineAdvTx-SplineAdvTn)
	
	
	nstation=length(station)
	
	Tn_mes_res <- Tn_mes -SplineAdvTn
	Tx_mes_res <- Tx_mes- SplineAdvTx 
	Tm_mes_res <- Tm_mes- SplineAdvTm 
	DeltaT_mes_res <- DeltaT_mes
	
	stdTn <- apply(X=Tn_mes_res,MARGIN=2,FUN=sd,na.rm=TRUE)
	stdTx <- apply(X=Tx_mes_res,MARGIN=2,FUN=sd,na.rm=TRUE)
	stdTm <- apply(X=Tm_mes_res,MARGIN=2,FUN=sd,na.rm=TRUE)
	
	
	for (s in 1:nstation) {
		
		Tn_mes_res[,s] <- (Tn_mes[,s]-SplineAdvTn[,s])/stdTn[s]	
		Tx_mes_res[,s] <- (Tx_mes[,s]-SplineAdvTx[,s])/stdTx[s]
		Tm_mes_res[,s] <- (Tm_mes[,s]-SplineAdvTm[,s])/stdTm[s]
		
		
	}
	
	
	if (option==1) {
		
		data_original <- cbind(Tx_mes_res,Tn_mes_res)
		
	} else if (option==2) {
		
		data_original <- cbind(Tm_mes_res,DeltaT_mes_res)
		normalize=TRUE
		
	} else if (option==3) {
		
		data_original <- Tm_mes_res
		# set data_original for option 3 
	
	} else { 
		data_original <- NULL
	}
	
	if (normalize) {
		
		data_for_var <- normalizeGaussian_severalstations(x=data_original,data=data_original,sample=sample,cpf=cpf,origin_x=origin,origin_data=origin)
		
	}  else {
		
		data_for_var <- data_original
		
	}
	
	
	
	
	out <- list(data_for_var,data_original,
			Tn_mes_res,Tx_mes_res,Tm_mes_res,DeltaT_mes_res,stdTn,stdTx,stdTm,
			SplineAdvTn,SplineAdvTx,SplineAdvTm,SplineAdvDeltaT,Tn_mes,Tx_mes,Tm_mes,DeltaT_mes,
			monthly_mean_Tx,monthly_mean_Tn)
	
	names(out) <- c("data_for_var","data_original",
			"Tn_mes_res","Tx_mes_res","Tm_mes_res","DeltaT_mes_res","stdTn","stdTx","stdTm",
			"SplineAdvTn","SplineAdvTx","SplineAdvTm","SplineAdvDeltaT","Tn_mes","Tx_mes","Tm_mes","DeltaT_mes",
			"monthly_mean_Tx","monthly_mean_Tn")
	
	return(out)
	
}


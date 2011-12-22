NULL
#' 
#' The comprehensive Temperature Generator
#' 
#' @param station see respective input parameter on \code{\link{setComprehensiveTemperatureGeneratorParameters}}
#' @param Tx_all,Tn_all,mean_climate_Tn,mean_climate_Tx see respective input parameter on \code{\link{setComprehensiveTemperatureGeneratorParameters}}
#' @param year_max,year_min,leap,nmonth,verbose see respective input parameter on \code{\link{setComprehensiveTemperatureGeneratorParameters}}
#' @param p,type,lag.max,ic,activateVARselect see respective input parameter on \code{\link{getVARmodel}}
#' @param year_max_sim last year of the simulation period. Default is equal to \code{year_max} 
#' @param year_min_sim fist year of the simulation period. Default is equal to \code{year_min}
#' @param mean_climate_Tn_sim monthly avaraged daily minimum temperatures for the simulated scenario and used by the random generator .  Default is \code{mean_climate_Tn}
#' @param mean_climate_Tx_sim monthly avaraged daily maximum temperatures for the simulated scenario and used by the random generator .  Default is \code{mean_climate_Tx}
#' @param onlygeneration logical variable. If \code{TRUE} the VAR model \code{varmodel} is given as input and only random generation is done, otherwise (default) is calculated from measured data 
#' @param varmodel the VAR model as a \code{varest} object. If \code{NULL}, it is  given as input and only random generation is done, otherwise (default) is calculated from measured data 
#' @param normalize,sample,extremes see \code{\link{normalizeGaussian_severalstations}} or \code{\link{setComprehensiveTemperatureGeneratorParameters}}
#' @param type_quantile see \code{type} on \code{\link{quantile}}
#' @param option integer value. If 1, the generator works with minimun and maximum temperature, if 2 (default) it works with the average value between maximum and minimum temparature and the respective daily thermal range.
#' @param n_GPCA_iteration number of iteration of Gaussianization process for data. Default is 0 (no Gaussianization) 
#' @param n_GPCA_iteration_residuals number of iteration of Gaussianization process for data. Default is 0 (no Gaussianization)
#' @param exogen matrix containing the (normalized or not) exogenous variables (predictors) for the recorded (calibration) period. Default is NULL
#' @param exogen_sim  matrix containing the (normalized or not) exogenous variables (predictors) for the simulation period. Default is \code{exogen}
#' @param is_exogen_gaussian logical value, If \code{TRUE}, \code{exogen_sim} and \code{exogen} are given as already normalized variables, otherwhise they are not normalized. Default is \code{FALSE}
#' @param exogen_all data frame containing exogenous variable formatted like \code{Tx_all} and {Tn_all}. Default is \code{NULL}. 
#' It is alternative to \code{exogen} and if it not \code{NULL},\code{is_exogen_gaussian} is automatically set \code{FALSE}	
#' @param exogen_all_col vector of considered  columns of \code{exogen_all}. Default is \code{station}.
#' 
#' @export 
#' 
#' @author  Emanuele Cordano, Emanuele Eccel
#'    
#' @seealso \code{\link{setComprehensiveTemperatureGeneratorParameters}}, \code{\link{generateTemperatureTimeseries}} ,\code{\link{generateTemperatureTimeseries}}. 
#' 
#'        
#'
#' @note It pre-processes series and generates multi-site temperature fields by using \code{\link{setComprehensiveTemperatureGeneratorParameters}},\code{\link{getVARmodel}} and \code{\link{generateTemperatureTimeseries}}. 
#' 
#'  
#' @return  A list of the following variables: 
#' 
#' \code{input}   list of variables returned by  \code{\link{setComprehensiveTemperatureGeneratorParameters}}
#' 
#' \code{var}     varest object containing the used VAR model (if useVAR is true), NULL (otherwise)
#' 
#' \code{output}  list variables returned by  \code{\link{generateTemperatureTimeseries}} (i.e. generated timeseries)
#' 
#' 
#' 
#' @examples 
#' data(trentino)
#' set.seed(1233)
#' vstation <- c("B2440","B6130","B8570","B9100","LAVIO","POLSA","SMICH","T0001",
#' "T0010","T0014","T0018","T0032","T0064","T0083","T0090","T0092","T0094","T0099",
#' "T0102","T0110","T0129","T0139","T0147","T0149","T0152","T0157","T0168","T0179","T0189","T0193","T0204","T0210","T0211","T0327","T0367","T0373")		
#' 
#' generation0 <- ComprehensiveTemperatureGenerator(station=vstation[5:8],Tx_all=TEMPERATURE_MAX,Tn_all=TEMPERATURE_MIN,year_min=1961,year_max=1990)
#' VAR_model <- generation0$var
#' CX <- generation0$input$monthly_mean_Tx
#' CN <- generation0$input$monthly_mean_Tn
#' generation1 <- ComprehensiveTemperatureGenerator(station=vstation[5:8],varmodel=generation0$var,
#' onlygeneration=TRUE,year_min=1961,year_max=1990,mean_climate_Tn_sim=CN,mean_climate_Tx_sim=CX,year_min_sim=1961,year_max_sim=1990,
#' Tx_all=TEMPERATURE_MAX,Tn_all=TEMPERATURE_MIN)
#'
#' str(generation0)
#' str(generation1)


	

ComprehensiveTemperatureGenerator <-
function(
		station=c("T0001","T0010","T0099"),
		Tx_all,
		Tn_all,
		mean_climate_Tn=NULL,
		mean_climate_Tx=NULL,
		year_max=1990,
		year_min=1961,
		leap=TRUE,
		nmonth=12,
		verbose=TRUE,
		p=1,
		type="none",
		lag.max=NULL,
		ic="AIC",
		activateVARselect=FALSE,
		year_max_sim=year_max,
		year_min_sim=year_min,
		mean_climate_Tn_sim=NULL,
		mean_climate_Tx_sim=NULL,
		onlygeneration=FALSE,
		varmodel=NULL,normalize=TRUE,
		type_quantile=3,
		sample=NULL,
		extremes=TRUE,
		option=2,
		n_GPCA_iteration=0,
		n_GPCA_iteration_residuals=n_GPCA_iteration,
		exogen=NULL,
		exogen_sim=exogen,
		is_exogen_gaussian=FALSE,
		exogen_all=NULL,
		exogen_all_col=station
	
) {
	

	
	useVAR=TRUE	
	if (option==2) normalize=TRUE # set NORMALIZE TRUE always for option=2
	
	origin <- paste(year_min,"1","1",sep="/") # Must start from Jan 1 
	origin_sim <- paste(year_min_sim,"1","1",sep="/") # Must start from Jan 1 
	
	
#	if (!onlygeneration){
	param <- setComprehensiveTemperatureGeneratorParameters(station=station,
				Tx_all=Tx_all,
				Tn_all=Tn_all,
				mean_climate_Tn=mean_climate_Tn,
				mean_climate_Tx=mean_climate_Tx,
				year_max=year_max,
				year_min=year_min,
				leap=leap,
				nmonth=nmonth,
				verbose=verbose,
				cpf=NULL,
				normalize=normalize,sample=sample,option=option)
		
		# PUT HERE CHECK FOR exogen 
	if (!onlygeneration){
		if (!is.null(exogen_all)) {
			
			 exogen <- as.data.frame(extractyears(exogen_all,year_min=year_min,year_max=year_max,station=exogen_all_col))
			 is_exogen_gaussian=FALSE
			 if (is.null(exogen_sim)) exogen_sim <- exogen
			
		}
		if (!is.null(exogen) & (!is_exogen_gaussian))  {
					
			exogen0 <- exogen
			exogen <- normalizeGaussian_severalstations(x=exogen0,data=exogen0,sample=sample,cpf=NULL,origin_x=origin,origin_data=origin,extremes=extremes)					
			
			
		}	
		var <- getVARmodel(data=param[['data_for_var']],suffix=c("_T1","_T2"),sep="",p=p,type=type,lag.max=lag.max,ic=ic,activateVARselect=activateVARselect,exogen=exogen,n_GPCA_iteration_residuals=n_GPCA_iteration_residuals,n_GPCA_iteration=n_GPCA_iteration,extremes=extremes) 
		
		if (activateVARselect) return(list(input=param,varselect=var))

	} else {

		var <- varmodel
	}
	
	
	
	if (is.null(mean_climate_Tn_sim)) mean_climate_Tn_sim <- param[['monthly_mean_Tn']]
	if (is.null(mean_climate_Tx_sim)) mean_climate_Tx_sim <- param[['monthly_mean_Tx']]
	
	
	nyear_sim <- year_max_sim-year_min_sim+1
	
	SplineAdvTx_sim <- as.data.frame(splineInterpolateMonthlytoDailyforSeveralYears(val=mean_climate_Tx_sim,start_year=year_min_sim,nyear=nyear_sim,leap=leap))
	SplineAdvTn_sim <- as.data.frame(splineInterpolateMonthlytoDailyforSeveralYears(val=mean_climate_Tn_sim,start_year=year_min_sim,nyear=nyear_sim,leap=leap))
	
	
	
	names(SplineAdvTx_sim) <- names(mean_climate_Tx_sim)
	names(SplineAdvTn_sim) <- names(mean_climate_Tn_sim) 
	
	SplineAdvTm_sim <- (SplineAdvTx_sim+SplineAdvTn_sim)/2.0
	SplineAdvDeltaT_sim <- (SplineAdvTx_sim-SplineAdvTn_sim)
	if (!is.null(exogen_sim) & (!is_exogen_gaussian))  {
		
		exogen0_sim <- exogen_sim
		exogen_sim <- normalizeGaussian_severalstations(x=exogen0_sim,data=exogen0_sim,sample=sample,cpf=NULL,origin_x=origin_sim,origin_data=origin_sim,extremes=extremes)					
	}
		
	results <- generateTemperatureTimeseries(std_tn=param[['stdTn']],std_tx=param[['stdTx']],SplineTx=SplineAdvTx_sim,SplineTn=SplineAdvTn_sim,SplineTm=SplineAdvTm_sim,SplineDeltaT=SplineAdvDeltaT_sim,std_tm=param[['stdTm']],var=var,normalize=normalize,type=type_quantile,sample=sample,option=option,original_data=param[['data_original']],origin_x=origin_sim,origin_data=origin,exogen=exogen_sim,extremes=extremes)	
	

	if (onlygeneration) {
		
		return(list(output=results))
		
	} else {
		
		return(list(input=param,var=var,output=results))
		
	}
	return(list(input=param,var=var,output=results))	
	
	
}


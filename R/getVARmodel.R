
NULL


#'  
#' Either creates an VAR model or chooses a VAR model by using VAR or VARselect commands of \code{vars} package  
#' 
#' @author Emanuele Cordano, Emanuele Eccel
#'    
#' @param data see \code{\link{VAR}} and \code{\link{addsuffixes}}
#' @param suffix see  \code{\link{addsuffixes}}
#' @param sep  separator element for 
#' @param p  lag considered for the auto-regression   see \code{\link{VAR}}
#' @param type see \code{\link{VAR}}
#' @param season see \code{\link{VAR}}
#' @param exogen see \code{\link{VAR}}
#' @param lag.max see \code{\link{VARselect}}
#' @param ic see \code{\link{VAR}}
#' @param activateVARselect logical variables. If \code{TRUE}, the function \code{\link{VARselect}} is run. Default and recommend use is \code{FALSE}.
#' @param na.rm logical variables. If \code{TRUE} (default), it takes into account \code{NA} values
#' @param n_GPCA_iteration number of iteration of Gaussianization process for data. Defauli is 0 (no Gaussianization) 
#' @param n_GPCA_iteration_residuals number of iteration of Gaussianization process for data. Defauli is 0 (no Gaussianization)
#' @param extremes  see \code{\link{normalizeGaussian_severalstations}} and \code{\link{GPCA}}
#' 
#' 
#' @export
#' 
#' @note  It inherits input parameters of \code{\link{VAR}}, \code{\link{VARselect}} and \code{\link{addsuffixes}}. The variable \code{data} contains the measured data on which the vector auto-regressive models is estimated.
#'  It is a matrix where each row is a realization of the vector random variable. 
#' 	In some application of this package, the random variables may be the daily maximum and minimum temperature anomalies for different stations. 
#' 	Often the the columns of \code{data} are called with the IDs of the stations whithout specifying the type of variable (e.g. minimun or maximum temperature anomalies). 
#' This means that two or more columns may have the same name. Therefore the function \code{\link{addsuffixes}}, which is called from this function, adds suitable suffixes to the column names. 
#'  
#'
#'        
#' summary(var)
#' 
#' @return  a \code{varest2} or \code{GPCAvarest2} object representing a VAR model or a \code{GPCA-varest} object which also contains the GPCA transformation parameters




getVARmodel <-
function (data,suffix=c("_Tx","_Tn"),sep="",p=1,type="none",season=NULL,exogen=NULL,lag.max=NULL,ic="AIC",activateVARselect=FALSE,na.rm=TRUE,
		n_GPCA_iteration=0,n_GPCA_iteration_residuals=n_GPCA_iteration,extremes=TRUE) { 
	
	if (!is.null(suffix)) names(data) <- addsuffixes(names=names(data),suffix=suffix,sep=sep) 
	
	if (na.rm) {
		
		data_old <- data
		data <- removeNAs(data_old)
		
		
	}
	
	if (!is.null(exogen)) exogen <- exogen[!is.na(data[,1]),]
	data <- data[!is.na(data[,1]),]
	
	if ((n_GPCA_iteration>0) | (n_GPCA_iteration_residuals>0)) {
		
		out <- new("GPCAvarest2")
		out@GPCA_data <- GPCA(x_prev=data,extremes=extremes,n=n_GPCA_iteration) 
		data_for_var <- out@GPCA_data$final_results
		
		
	} else {
		
		out <- new("varest2")
		data_for_var <- data
		
	}
	
	
	if (activateVARselect) {

		if (is.null(lag.max)) lag.max=10
#		varmodel <- VARselect(data_for_var,type=type,season=season,exogen=exogen,lag.max=lag.max)
		varselect <- VARselect(data_for_var,type=type,season=season,exogen=exogen,lag.max=lag.max)
		return(varselect) # Return the varselect list 
	} else {
		if (!is.null(exogen)) exogen <- exogen[!is.na(data[,1]),]
#		varmodel <- VAR_list(y=data_for_var,p=p,type=type,season=season,exogen=exogen,lag.max=lag.max,ic=ic)
		out@VAR <- VAR(y=data_for_var,p=p,type=type,season=season,exogen=exogen,lag.max=lag.max,ic=ic)
	}
	
	if (n_GPCA_iteration_residuals>0) {
		
		out@GPCA_residuals <- GPCA(x_prev=out,extremes=extremes,n=n_GPCA_iteration) 
	
	
	}
	
	# TO GO ON !!!
	
	return(out)
	
}


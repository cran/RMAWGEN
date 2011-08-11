
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
#' 
#' @note  It inherits input parameters of \code{\link{VAR}}, \code{\link{VARselect}} and \code{\link{addsuffixes}}. The variable \code{data} contains the measured data on which the vector auto-regressive models is estimated.
#'  It is a matrix where each row is a realization of the vector random variable. 
#' 	In some application of this package, the random variables may be the daily maximum and minimum temperature anomalies for different stations. 
#' 	Often the the columns of \code{data} are called with the IDs of the stations whithout specifying the type of variable (e.g. minimun or maximum temperature anomalies). 
#' This means that two or more columns may have the same name. Therefore the function \code{\link{addsuffixes}}, which is called from this function, adds suitable suffixes to the column names. 
#'  
#'
#' @callGraphPrimitives      
#' 
#' 
#' @return  a varest object representing a VAR model




getVARmodel <-
function (data,suffix=c("_Tx","_Tn"),sep="",p=1,type="none",season=NULL,exogen=NULL,lag.max=NULL,ic="AIC",activateVARselect=FALSE,na.rm=TRUE) { 

	if (!is.null(suffix)) names(data) <- addsuffixes(names=names(data),suffix=suffix,sep=sep) 
	
	if (na.rm) {
		
		data_old <- data
		data <- removeNAs(data_old)
		
		
	}
	
	
	if (activateVARselect) {
	
		out <- VARselect(data[!is.na(data[,1]),],lag.max=lag.max)
		
	} else {
		if (!is.null(exogen)) exogen <- exogen[!is.na(data[,1]),]
		out <- VAR(y=data[!is.na(data[,1]),],p=p,type=type,season=season,exogen=exogen,lag.max=lag.max,ic=ic)
		
	}
	
	
	
	
	
	return(out)
	
}


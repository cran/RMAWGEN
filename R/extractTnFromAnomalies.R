NULL


#'  
#' Extracts generated time series of Daily Minimum Temperature from a random multi-realization obtained by \code{\link{generateTemperatureTimeseries}} function 
#' 
#' @param res_multigen matrix containing standardized values of daily temperature as returned by \code{\link{generateTemperatureTimeseries}} (first item)
#' @param std vector containing start deviation for each minimun temperature anomalies
#' @param SplineAdv matrix containing the averaged daily values of minimun temperature obtained by a spline interpolation of the monthly climate
#' 
#' 
#' 
#' 
#' @author  Emanuele Cordano, Emanuele Eccel
#'    
#'   
#'
#' @callGraphPrimitives      
#' 
#' @return  a matrix with generated minimum temperature




extractTnFromAnomalies <-
function (res_multigen,std,SplineAdv) {

	
	ntall <- as.integer(ncol(res_multigen))
	ntn <- as.integer(ncol(res_multigen)/2)
	
	
	out <- res_multigen[,(ntn+1):ntall]%*% diag(std) + SplineAdv
	return(out)
	
}


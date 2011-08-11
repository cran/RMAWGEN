
NULL


 
#'   
#' Generates a new realization of a VAR model 
#'  
#'  
#'
#' @param var A VAR model represented by a \code{varest} object as returned by \code{\link{getVARmodel}} or \code{\link{VAR}}
#' @param xprev previous status of the random variable
#' @param exogen vector containing the values of the "exogen" variables (predictor) for the generation
#' @param B matrix of coefficients for the vectorial white-noise component
#' 
#' @callGraphPrimitives     
#' 
#' @author  Emanuele Cordano, Emanuele Eccel
#' @seealso \code{\link{forecastEV}},\code{\link{forecastResidual}}
#' 
#' 
#' 
#' @return  a vector of values




NewVAReventRealization <-
function(var,xprev,exogen=NULL,B=NULL) {
	
	
	
	
	
	out <- forecastEV(var=var,xprev=xprev,exogen=exogen)+forecastResidual(var=var,B=B)
	
	return(out)
	
}


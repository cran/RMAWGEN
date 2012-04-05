NULL 
#' 
#' 
#'\code{residuals} S3 method for \code{varest2} object
#'
#' @param object a \code{blockmatrix} object 
#' @param ...   passed arguments  
#' @export
#' @rdname residuals
#' @method residuals varest2 
#' @S3method residuals varest2
#' @aliases residuals
#'
#' @return residuals of blockmatrix \code{object} 
#' 
#' @author Emanuele Cordano 
#' 
#' 



residuals.varest2 <- function(object,...) {
	
#	temp <- object@VAR
	
	out <- residuals(object@VAR)
	if (class(object)=="GPCAvarest2") {
		if (length(object@GPCA_residuals)>0) {
			out <- object@GPCA_residuals$final_results
		}
	}
    
	return(out)
	
}



NULL
#'
#' This function makes an inverse Gaussianization procedure besad on PCA iteration ( see \code{\link{inv_GPCA_iteration}}
#'
#' @param x gaussian random variable to transform 
#' @param GPCA_param \code{\link{GPCA-class}} S3 object returned by the function \code{\link{GPCA}}
#' @param extremes see \code{\link{normalizeGaussian_severalstations}}
#' @param type see \code{\link{normalizeGaussian_severalstations}}
#' 
#' @author Emanuele Cordano 
#' 
#' @export 
#' @return the non-Gaussian random variable
#' 
#' 
#' @note This function re-iterates the inverse of equation (1) of "PCA Gaussianization for One-Class Remote Sensing Image" by V. Laparra et al.,  \url{http://dx.doi.org/doi/10.1117/12.834011} 




inv_GPCA <- function (x=NULL,GPCA_param,type=3,extremes=TRUE) {
	
	
	
	n <- length(GPCA_param)-1
	
	if (n<=0) return(x)
	if (is.null(x)) x <- GPCA_param$final_results
	
	for (i in 1:n) {
	
		out <- inv_GPCA_iteration(x,GPCA_param[[n-i+1]],type=type,extremes=TRUE)
		x <- out
	}
	

	return(out)

}


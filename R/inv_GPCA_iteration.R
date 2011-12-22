NULL
#'
#' This function makes an inverse iteration of PCA-Gaussianization process 
#'
#' @param x matrix of gaussian random variale to transform
#' @param GPCA_iter_param \code{\link{GPCAiteration}} S3 object returned by the function \code{\link{GPCA_iteration}} corresponding the related direct iteration
#' @param extremes see \code{\link{normalizeGaussian_severalstations}}
#' @param type see \code{\link{normalizeGaussian_severalstations}}
#'
#' @export 
#' 
#' @return the non-Gaussian random variable
#' 
#' @note This function is based on the inverse of the equation (1) of "PCA Gaussianization for One-Class Remote Sensing Image" by V. Laparra et al.,  \url{http://dx.doi.org/doi/10.1117/12.834011} 

inv_GPCA_iteration <- function (x=GPCA_iter_param$x_next,GPCA_iter_param,type=3,extremes=TRUE) {
	
	
# The iterartion is formulated in eq. 1 of \url{www.uv.es/lapeva/papers/SPIE09_one_class.pdf}	
	
	M <- t(GPCA_iter_param$B_prev) 
	
	y <- M %*% t(as.matrix(x))
	
	out <- normalizeGaussian_severalstations(x=as.data.frame(t(y)),data=GPCA_iter_param$x_prev,type=type,extremes=extremes,inverse=TRUE)

	return(out)
	
}

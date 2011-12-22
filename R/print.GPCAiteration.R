NULL 
#' 
#'\code{print} S3 method for \code{GPCAiteretion} object
#'
#' @param x a \code{GPCAiteration} object 
#' @param rmin,rmax,cmin,cmax maximum and minimum rows and columns to be printed 
#' @param ...   passed arguments  
#' 
#' @export
#' 
#' @seealso \code{\link{GPCA_iteration}}
#' 

print.GPCAiteration <- function(x,rmin=1,rmax=4,cmin=rmin,cmax=rmax,...) {
	

	print("GPCA Iteration, matrix rotation:")
	print(x$B_prev[rmin:rmax,cmin:cmax])

	return(0)
	
}



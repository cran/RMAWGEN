NULL 
#' 
#'\code{print} S3 method for \code{GPCA} object
#'
#' @param x a \code{GPCA} object 
#' @param rmin,rmax,cmin,cmax maximum and minimum rows and columns to be printed 
#' @param ...   passed arguments  
#' 
#' @export
#' 
#' @seealso \code{\link{GPCA}}
#' 

print.GPCA <- function (x,rmin=1,rmax=4,cmin=rmin,cmax=rmax,...) {
	
	print("GPCA S3 Object:")
	if (length(x)>1) {
		
		for (i in 1:(length(x)-1)) {
			
			iteration <- paste ("Iteration n:",i,sep=" ")
			print(iteration)
			print(x[[i]],rmin=rmin,rmax=rmax,cmin=cmin,cmax=cmax,...)
			
		}
	} 

	return(0)
	
}



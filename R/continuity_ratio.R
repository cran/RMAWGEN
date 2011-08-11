NULL
#'
#'  Calculates the continuity ratio of a set of precipitation measured or generated data in several sites as defined by Wilks, 1998 (see reference link)
#' 
#' @param data containing daily precipitation time series for several gauges (one gauge time series per column)
#' @param valmin threshold precipitation value [mm] for wet/dry day indicator. 
#' If precipitation is lower than \code{valmin}, day is considered dry. Default is 0.5 mm.
#' 
#' @author Emanuele Cordano, Emanuele Eccel 
#' 
#' @references  see the following URL references:  \url{http://onlinelibrary.wiley.com/doi/10.1002/joc.2305/abstract} 
#'   and \url{http://www.sciencedirect.com/science/article/pii/S0022169498001863}
#' @return the value of the continuity ratio 
#' 




continuity_ratio <- function(data,valmin=0.5) {
	
	ncols <- ncol(data)
	out <- array(NA,c(ncols,ncols))
	for (i in 1:ncols) {
		for (j in 1:ncols) {
			
			d1 <- data[,i]
			d2 <- data[,j]
			e1 <- mean(d1[d1>valmin & d2>valmin],na.rm=TRUE)
			e2 <- mean(d1[d1>valmin & d2<=valmin],na.rm=TRUE)
			l1 <- length(d1[d1>valmin & d2>valmin])
			l2 <- length(d1[d1>valmin & d2<=valmin])
	
			
			out[i,j] <- e2/e1
			
		}
		
	}
	
	return(out)
}
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
#' @export 
#' 
#' @references  see the following URL references:  \url{http://onlinelibrary.wiley.com/doi/10.1002/joc.2305/abstract} 
#'   and \url{http://www.sciencedirect.com/science/article/pii/S0022169498001863}
#' @return the value of the continuity ratio 
#' 






continuity_ratio <- function(data,valmin=0.5) {
	
	ncols <- ncol(data)
	nrows <- nrow(data)
	out <- new.env()
	out$continuity_ratio <- array(NA,c(ncols,ncols))
	out$occurence <- array(NA,c(ncols,ncols))
	out$nooccurence <- array(NA,c(ncols,ncols))
	for (i in 1:ncols) {
		for (j in 1:ncols) {
			
			d1 <- data[,i]
			d2 <- data[,j]
			e1 <- mean(d1[d1>valmin & d2>valmin],na.rm=TRUE)
			e2 <- mean(d1[d1>valmin & d2<=valmin],na.rm=TRUE)
			l1 <- length(d1[d1>valmin & d2>valmin])
			l2 <- length(d1[d1>valmin & d2<=valmin])
	
			nrowsa <- length(d1[!is.na(d1) & !is.na(d2)])

			out$continuity_ratio[i,j] <- e2/e1
			out$occurence[i,j] <- length(d1[d1>valmin & d2>valmin & !is.na(d1) & !is.na(d2)])/nrowsa
			out$nooccurence[i,j] <- length(d1[d1<=valmin & d2<=valmin & !is.na(d1) & !is.na(d2)])/nrowsa
			
		}
		
	}
	
	return(as.list(out))
}
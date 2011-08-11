NULL


#' 
#' Extracts the rows of a matrix corresponding to requested months of a year given the  date (origin) of the first row 
#' 
#'  @author  Emanuele Cordano, Emanuele Eccel
#'
#' @param data an input data matrix where each row corresponds to a daily record
#' @param when character vactor of months for which the data are required. 
#' It must be a subset of \code{c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")}
#' @param origin date corresponding to the first row of \code{data}
#' @param ndim_max maximimum (integer) number of rows in \code{data} where to find \code{when}. 
#' Default is 100000 and works if \code{data} is missing.
#' 
#'     
#'   
#'
#' @return   a matrix containing the requested rows
#' @callGraphPrimitives    
#' @note It uses \code{\link{months}} and  \code{\link{julian}}
#' @seealso \code{\link{extractdays}}
#' 
#' 






extractmonths <-
function(data=array(1:ndim_max,dim=c(ndim_max,1)),ndim_max=100000,when=c("Dec","Jan","Feb"),origin="1961-1-1"){
	
	out <- NULL
	if (class(data)=="matrix" | class(data)=="data.frame" ) {
		ndata=nrow(data) 
	
		start <- as.integer(julian(as.POSIXct(origin,tz="GMT")))
	
		out <- (data[which(months(1:ndata+start-1,abbreviate=TRUE) %in% when) ,])
	} else {
		
		ndata=length(data) 
		
		start <- as.integer(julian(as.POSIXct(origin),tz="GMT"))
		
		out <-  (data[which(months(1:ndata+start-1,abbreviate=TRUE) %in% when) ])
		
	}
	
	
	return(out)
}


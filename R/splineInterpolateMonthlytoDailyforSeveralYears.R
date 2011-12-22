NULL


#' 
#' Interpolates monthly data to daily data using \code{\link{splineInterpolateMonthlytoDaily}}  for several years
#' 
#' 
#' @param val matrix containing monthly mean data for one year
#' @param start_year first year
#' @param nyear number of years since \code{start_year}
#' @param leap logical variable If \code{TRUE} (default) leap years are considered, otherwise they are not
#' @param offset integer values. Default is 2. Number of years considered beyond the extremes in order to avoid edge errors 
#' 
#' @return a matrix with interpolated daily data 
#' 
#' @export 
#' 
#' @seealso \code{\link{spline}},\code{\link{splineInterpolateMonthlytoDaily}}
#' 
#' @author Emanuele Cordano, Emanuele Eccel
#' 
#' 





splineInterpolateMonthlytoDailyforSeveralYears <-
function(val,start_year=2010,nyear=1,leap=TRUE,offset=2) {
	
	
	
	tval <- val 
	
	
	nyear_sim=nyear+2*offset
	
	if (nyear_sim>1) for (i in 2:nyear_sim) tval <- rbind(tval,val)
	
	
	
	
	end_year=start_year+nyear-1
	
	count=0
	offset_day=0
	count1=0
	for (year in (start_year-offset):(end_year+offset)) {
		
		if (leap & leap.year(year)) {
		
			count=count+366
			if (year<start_year) offset_day=offset_day+366
			if (year<=end_year) count1=count1+366
		} else {

			count=count+365	
			if (year<start_year) offset_day=offset_day+365
			if (year<=end_year) count1=count1+365
		}
		
	}
#	print(offset_day)
	nday=count
	nday1=count1
#	offset_day=offset*365
#	nday=count+2*offset_day
	
	origin <- paste(start_year-offset,"1","1",sep="/")
	
	output <- splineInterpolateMonthlytoDaily(nday=nday,val=tval,origin=origin,first_row=offset_day+1,last_row=nday1)
	
	
	
	return(output[(offset_day+1):nday1,])

}


NULL


#'  
#' Generates several realizations of a VAR model 
#' 
#' 
#' @author  Emanuele Cordano, Emanuele Eccel
#' 
#' @param var A VAR model represented by a \code{varest} object as returned by \code{\link{getVARmodel}} or \code{\link{VAR}}
#' @param xprev previous status of the random variable
#' @param exogen matrix containing the values of the "exogn" variables (predictor) for the generation
#' @param nrealization number of realization (e.g. days to simulate). If \code{exogen} is not \code{NULL} and it is a matrix, it must be lower or equal to the number of rows of \code{exogen}
#' @param B matrix of coefficients for the vectorial white-noise component
#' 
#' 
#'    
#'  
#'
#' @callGraphPrimitives     
#' 
#' @return  a matrix of values





newVARmultieventRealization <-
function(var,xprev=rnorm(var$K*var$p),exogen=NULL,nrealization=10,B=t(chol(summary(var)$covres))) {
	

	
	
	K <-var$K
	p <- var$p
	
	nexogen <- ncol(var$datamat)-var$K*(var$p+1)
	
	if ((is.null(exogen)) & (nexogen!=0)) {
		print("Error exogen variables (predictors) are needed to new VAR multirealization") 
	} else if (!is.null(exogen)) if (nexogen!=ncol(exogen)) print("Error corrected exogen variables (predictors) are needed to new VAR multirealization") 
	#if (nexogen!=ncol(exogen)) names
	
	out <- array(NA,c(nrealization,K))
	
	x <-xprev[1:K]
	if (p>1) xprev <- xprev[(K+1):(K*p)]

	
	for(i in 1:nrow(out)) {
		
	
		if (p>1) xprev <- c(x,xprev[1:(K*(p-1))]) else xprev <- x
	
		if (is.null(exogen)) {
			exogen_data <- NULL
		} else {
			exogen_data <- as.vector(t(exogen[i,]))
		}
		
		x <- NewVAReventRealization(var=var,xprev=xprev,exogen=exogen_data,B=B)
		out[i,] <- x
		
		
	}
	
	
	return(out)
	
	
}


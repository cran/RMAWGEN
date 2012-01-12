NULL


#'  
#' Generates several realizations of a VAR model 
#' 
#' 
#' @author  Emanuele Cordano, Emanuele Eccel
#' 
#' @param var A VAR model represented by a \code{varest2} object as returned by \code{\link{getVARmodel}}
#' @param xprev previous status of the random variable
#' @param exogen matrix containing the values of the "exogen" variables (predictor) for the generation
#' @param nrealization number of realization (e.g. days to simulate). If \code{exogen} is not \code{NULL} and it is a matrix, it must be lower or equal to the number of rows of \code{exogen}
#' @param B matrix of coefficients for the vectorial white-noise component
#' @param extremes,type  see \code{\link{inv_GPCA}}
#'    
#' @export  
#'
#'       
#' 
#' @return  a matrix of values


# TO ADJUST 


newVARmultieventRealization <-
function(var,xprev=rnorm(var@VAR$K*var@VAR$p),exogen=NULL,nrealization=10,B=t(chol(summary(var@VAR)$covres)),extremes=TRUE,type=3) {
	

	
	
	K <-var@VAR$K
	p <- var@VAR$p
	
	nexogen <- ncol(var@VAR$datamat)-K*(p+1)
	noise <- array(rnorm(K*nrealization),c(nrealization,K))
	
	
	if (class(var)=="GPCAvarest2") {
		
		noise1 <- as.data.frame(t(B %*% t(as.matrix(noise))))
		
		noise <- inv_GPCA(x=noise1,GPCA_param=var@GPCA_residuals,type=type,extremes=extremes)
		B <- diag(1,ncol(B))
	} 
		
	
	
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

		x <- NewVAReventRealization(var=var@VAR,xprev=xprev,exogen=exogen_data,noise=as.vector(t(noise[i,])),B=B)
		out[i,] <- x
	
		
	}
	
	if (class(var)=="GPCAvarest2") {
	
		out <- inv_GPCA(x=out,GPCA_param=var@GPCA_data,type=type,extremes=extremes)
		
	} 		
	
	return(as.matrix(out))
	
	
}


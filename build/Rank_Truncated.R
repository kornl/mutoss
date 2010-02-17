# 
# 
# Author: FrankKonietschke
###############################################################################

#' Rank truncated p-Value procedure
#' The program computes the exact distribution and with it the p-Value
#' 
#' This function computes the exact distribution of the product of
#' at most K significant p-values of $L>K$ observed p-values. Thus, one get the
#' pvalue from the exact distribution. This has certain advantages for genomewide 
#' association scans: K can be chosen on the basis of a hypothesised 
#' disease model, and is independent of sample size. Furthermore, 
#' the alternative hypothesis corresponds more closely to the 
#' experimental situation where all loci have fixed effects.
#' 
#' @param pValues Vector of p-Values (not sorted)
#' @param K the number of hypotheses / p-Values being in w
#' @param silent If true any output on the console will be suppressed.  
#' @return Used.pValue: List information about the used pValues; RTP: Test statistic and pValue
#' @author Frank Konietschke
#' @references 	Dubridge, F., Koeleman, B.P.C. (2003). Rank truncated product of P-values, with application to genomewide association scans. Genet Epidemiol. 2003 Dec;25(4):360-6
#' @export
#' @examples 
#' 
#' pvalues<-runif(10)
#' result <- ranktruncated(pvalues,K=2,silent=FALSE) # take the K=2 smallest pvalues
#' result <- ranktruncated(pvalues,K=2,silent=TRUE) # take the K=2 smallest pvalues
#' result <- ranktruncated(pvalues,K=5,silent=TRUE) # take the K=5 smallest pvalues

ranktruncated <- function(pValues, K, silent = FALSE){
	
	L <- length(pValues)
	
	if (K > L){
		warn1 <- paste("K must be smaller than L")
		stop(warn1)
	}
	
#-----Compute the test statistic-----#
	
	index <- order(pValues)
	rindex <- order(index)
	spval <- pValues[index]
	
	w <- prod(spval[1:K])
	
	
#---Compute the used pvalues--------#
	
	w.pvalues <- pvalues[rindex]
	
	p.used <- data.frame(Position=index[1:K], pValue=spval[1:K])
	
	
	
	
	
#--Compute the function awt as in the paper-----#

awt <- function(w,t,K){
	if (w<=t^K){
		s <- c(0:(K-1))
		num1 <- K*log(t)-log(w)
		num2 <- w * sum(num1^s/factorial(s))
	}
	if (w > t^K) {
		num2<- t^K
	}
	return(num2)
}

# ----- Compute now the exact distribution-----#
	
	fac1 <- choose(L, K+1)*(K+1)
	t <- seq(0.001,0.999,0.001)
	terg <- c()
	
	for (i in 1:length(t)){
		terg[i] <- (1-t[i])^(L-K-1)*awt(w,t[i],K)
}

#--------------Compute the p-Value -------------#
distribution <- fac1*mean(terg)

#-Regarding the error of numerical integration-#
	
p1 <- (distribution>1)
distribution[p1] <- 1

#--------------Prepare the output---------------#

	if (! silent)
	{
		cat("#----Rank Truncated Product of P-Values (Dubridge and Koeleman; 2003)   \n\n")
			
	}
result <- data.frame(Statistic = w, p.Value=distribution)
return(list(Used.pValue=p.used, RTP=result))
}









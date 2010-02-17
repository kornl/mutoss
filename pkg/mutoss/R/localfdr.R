# 
# Author: JonathanRosenblatt
###############################################################################


#' The function \code{pval2qval} takes a vector of p-values and
#' estimates for each case the tail area-based FDR, which can be regarded as a p-value corrected for multiplicity. 
#' This is done by calling the \code{fdrtool} function.
#' If a cutoff is supplied, a vector of rejected hypotheses will be returned as well.
#' 
#' @title Strimmer et al.'s fdrtool-based q-values
#' @param pValues Numeric vector of p-values to be used.
#' @param cutoff The positive FDR cutoff for rejection. Hypotheses with \code{qValues} smaller then \code{cutoff} will be rejected.
#' @return A list containing:
#' \item{qValues}{A numeric vector with one q-value for each hypothesis.}
#' \item{rejected}{A logical vector indicating rejection/retention for each hypothesis when \code{cutoff} is supplied.}
#' @author JonathanRosenblatt
#' @references 
#' Strimmer, K. (2008). fdrtool: a versatile R package for estimating local and tail
#' area-based false discovery rates. Bioinformatics 24: 1461-1462. \cr
#' Storey, J. D. (2003)  The Positive False Discovery Rate: A Bayesian Interpretation and the q-Value.
#' The Annals of Statistics 31(6): 2013-2035
#' @examples 
#' pvals<- runif(1000)^2
#' pval2qval(pvals)
#' pval2qval(pValues=pvals, cutoff=0.1)
#' @export 

pval2qval<- function(pValues, cutoff){
	require('fdrtool')
	qvals<-fdrtool(
			pValues,
			statistic= 'pvalue', 
			plot=FALSE,verbose=FALSE)$qval
	
	if (missing(cutoff)) {
		return(list(qValues=qvals))
	}
	return(list(qValues=qvals, rejected= qvals<=cutoff ))		
	
}

#' The function \code{pval2locfdr} takes a vector of p-values and
#' estimates for each case the local fdr.
#' 
#' @title Strimmer et al.'s fdrtool-based local fdr
#' @param pValues pValues to be used.
#' @param cutoff The local fdr cutoff for rejection. Hypotheses with \code{local fdr} smaller then \code{cutoff} will be rejected.  
#' @return A list containing:
#' 
#' \item{locfdr}{Numeric vector with local FDR values for each case}
#' \item{rejected}{Logical vector indicating rejection/retention for each hypothesis when a \code{cutoff} is supplied.}
#' @author JonathanRosenblatt
#' @references 
#' Strimmer, K. (2008). fdrtool: a versatile R package for estimating local and tail
#' area-based false discovery rates. Bioinformatics 24: 1461-1462. \cr
#' Efron B., Tibshirani R., Storey J. D. and Tusher, V. (2001). Empirical Bayes Analysis of a Microarray Experiment.
#' Journal of the American Statistical Association 96(456):1151-1160.
#' @examples 
#' pvals<- runif(1000)^2
#' pval2locfdr(pvals)
#' pval2locfdr(pValues=pvals, cutoff=0.4)
#' @export 


pval2locfdr<- function(pValues, cutoff){
	require('fdrtool')
	locfdr<-fdrtool(
			pValues,
			statistic= 'pvalue', 
			plot=FALSE,verbose=FALSE)$lfdr
	
	if (missing(cutoff)) {
		return(list(locFDR=locfdr))
	}
	return(list(locFDR=locfdr, rejected= locfdr<=cutoff ))		
}

mutoss.locfdr <- function() { 
	return(new(Class="MutossMethod",
					label="Local FDR (fdr)",
					callFunction="pval2locfdr",
					output=c("locFDR", "rejected"),  
					info=
							"<h2> Name: </h2> Local fdr.\n
							<h3> Also known as: </h3> fdr, empirical posterior probability of the null. \n
							<h3> Error Type: </h3> Motivated by Bayesian considerations. Does not guarantee control of frequentist error types like FWER or FDR.\n
							<h3> Recommended Usage: </h3> Typically used when a massive amount of hypotheses is being tested as in microarray analyses.\n
							<h3> Related procedures: </h3> See FDR methods for similar procedures for frequentist error control.\n
							<h3> References: </h3> \n
							<ul>
							<li> Efron B., Tibshirani R., Storey J. D. and Tusher, V. (2001).<i> Empirical Bayes Analysis of a Microarray Experiment. </i>\n
								 Journal of the American Statistical Association 96(456):1151-1160. </li>
							</ul>",				
					parameters=list(
							pValues=list(type="numeric"),
							cutoff=list(type="numeric", label="Local fdr cutoff for rejection", optional=TRUE))))
}



mutoss.qvalues <- function() { 
	return(new(Class="MutossMethod",
					label="q Values (Fdr)",
					callFunction="pval2qval",
					output=c("qValues", "rejected"),
					info=
							"<h2> Name: </h2> q-Values.\n
							<h3> Also known as: </h3> \n
							<ul> 
								<li> Estimated pFDR</li>\n 
								<li> Estimated Positive FDR </li>\n
								<li> Empirical tail-area posterior probability of the null</li> \n
							<h3> Error Type: </h3> Motivated by Bayesian considerations. Guarantees FDR control only when masses of hypotheses are being tested.\n
							<h3> Recommended Usage: </h3> Typically used when a massive amount of hypotheses is being tested as in microarray analyses.\n
							<h3> Related procedures: </h3> See FDR methods for similar procedures with frequentist error control.\n
							<h3> References: </h3> \n
							<ul>
							<li> Storey, J. D. (2003)<i>The Positive False Discovery Rate: A Bayesian Interpretation and the q-Value.</i>
							 The Annals of Statistics 31(6): 2013-2035. </li>
							</ul>",
					parameters=list(
							pValues=list(type="numeric"),
							cutoff=list(type="numeric", label="q-value (pFDR) cutoff for rejection", optional=TRUE))))
}

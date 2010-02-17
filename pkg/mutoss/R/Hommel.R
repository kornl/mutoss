#' Hommel's step-up-procedure.
#' 
#' The method is applied to p-values. It controls
#' the FWER in the strong sense when the hypothesis tests are independent
#' or when they are non-negatively associated.
#' 
#' The method base upon the closure principle to assure the FWER alpha and 
#' the critical Values of this procedure are given by alpha/n,
#' alpha/(n-1), ..., alpha/2, alpha/1.
#' 
#' @title Hommel's (1988) step-up-procedure
#' @param pValues pValues to be used. They need a independent structure. 
#' @param alpha The level at which the FWER should be controlled
#' @param silent Logical. If true, any output on the console will be suppressed.  
#' @return A list containing:
#'   
#'	\item{adjPValues}{A numeric vector containing the adjusted pValues}
#' 
#'	\item{rejected}{A logical vector indicating which hypotheses are rejected}
#' 
#'	\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test.}
#' 
#' 	\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}
#' 
#' @author HackNiklas
#' @references G. Hommel (1988). A stagewise rejective multiple test procedure based on a modified Bonferroni test.
#' Biometrika 75, pp. 383-386 
#' @export
#' @examples 
#' pval <- c(runif(50), runif(50, 0, 0.01))
#' result 	<- hommel(pval, 0.05)
#' result 	<- hommel(pval, 0.05, silent = TRUE)
hommel <- function(pValues, alpha,silent=FALSE) {
	m <- length(pValues)
	criticalValues <- sapply(1:m, function(i) (i*alpha)/m)
	adjPValues <- p.adjust(pValues, "hommel")
	rejected <- adjPValues<=alpha
	if (! silent)
	{
		cat("\n\n\t\tHommel's (1988) step-up Procedure\n\n")
		printRejected(rejected, pValues, NULL)
	}
	return(list(adjPValues=adjPValues, criticalValues=criticalValues, rejected=rejected,
					errorControl = new(Class='ErrorControl',type="FWER",alpha=alpha)))
}
#' @export
mutoss.hommel <- function() { return(new(Class="MutossMethod",
					label="Hommel (1988) adjustment",
					errorControl="FWER",
					callFunction="hommel",
					output=c("adjPValues", "criticalValues", "rejected", "errorControl"),
					assumptions=c("any dependency structure"),
					info="<h2>Hommel (1988) adjustment </h2>\n\n\
							<h3>Reference:</h3>\
							<ul>\
							<li>Hommel, Gerhard. \"<i> A stagewise rejective multiple test procedure based on a modified Bonferroni test. </i>\" Biometrika 75, pp. 383-386, 1988. </li>\n\
							</ul>
							<p>The method is applied to pValues. It controls \
							the FWER in the strong sense when the hypothesis tests are independent \
							or when they are non-negatively associated. \
							The method base upon the closure principle to assure the FWER alpha and \ 
							the critical Values of this procedure are given by alpha/n, \
							alpha/(n-1), ..., alpha/2, alpha/1.</p>\n",
					parameters=list(pValues=list(type="numeric"), alpha=list(type="numeric"))
			)) }

#' Bejamini-Hochberg (2000) oracle linear step-up Procedure
#' 
#' Knowledge of the number of true null hypotheses (m0) can be very useful to improve upon the performance of the FDR controlling procedure. 
#' For the oracle linear step-up procedure we assume that m0 were given to us by an 'oracle', the linear step-up procedure with q0 = q*m/m0
#' would control the FDR at precisely the desired level q in the independent and continuous case, and
#' would then be more powerful in rejecting hypotheses for which the alternative holds.
#' 
#' @param pValues pValues to be used
#' @param alpha the level at which the FWER should be controlled
#' @param pi0 miraculousy known number of true null hypotheses
#' @param silent Logical, if true any output on the console will be suppressed.  
#' @return A list containing:
#' 
#'	\item{adjPValues}{A numeric vector containing the adjusted pValues}
#' 
#'	\item{rejected}{A logical vector indicating which hypotheses are rejected}
#' 
#'	\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test.}
#' 
#' 	\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}
#' 
#' @author HackNiklas
#' @export
#' @examples 
#' pval <- c(runif(50), runif(50, 0, 0.01))
#' result <- oracleBH(pValues=pval,alpha=0.05,pi0=0.85)
oracleBH <- function(pValues, alpha, pi0, silent=FALSE) {
	m <- length(pValues)
	adjPValues <- p.adjust(pValues,"BH")*pi0
	rejected <- (adjPValues <= alpha)
	criticalValues <- sapply(1:m, function(i) (i*alpha)/(pi0*m))
	if (! silent)
	{
		cat("\n\n\t\tBenjamini-Hochberg's (1995) oracle linear-step-up Procedure\n\n")
		printRejected(rejected, pValues, adjPValues)
	}
	return(list(adjPValues=adjPValues, criticalValues=criticalValues, rejected=rejected,
					errorControl = new(Class='ErrorControl',type="FDR",alpha=alpha)))
}
#' @export
mutoss.oracleBH <- function() { return(new(Class="MutossMethod",
					label="Benjamini-Hochberg (1995) oracle linear-step-up",
					errorControl="FDR",
					callFunction="oracleBH",
					output=c("adjPValues", "criticalValues", "rejected", "errorControl"),
					assumptions=c("test independence or positive regression dependency"),
					info="<h2>Benjamini-Hochberg (1995) oracle linear step-up Procedure </h2>\n\n\
							<h3>Reference:</h3>\
							<ul>\
							<li>Bejamini, Yoav and Hochberg, Josef. \"<i> Controlling the false discovery rate: a practical and powerful approach to multiple testing.
							</i>\" J. Roy. Statist. Soc. Ser. B 57 289-300, 1995. </li>\n\
							</ul>
							<p>Knowledge of the number of true null hypotheses (m0) can be very useful to improve upon the performance of the FDR controlling procedure. \
							For the oracle linear step-up procedure we assume that m0 were given to us by an `oracle', the linear step-up procedure with q0 = q*m/m0 \
							would control the FDR at precisely the desired level q in the independent and continuous case, and \
							would then be more powerful in rejecting hypotheses for which the alternative holds.</p>\n",
					parameters=list(pValues=list(type="numeric"), alpha=list(type="numeric"), pi0=list(type="numeric"))
			)) }

#' Storey's (2001) q-value Procedure
#' 
#' The Qvalue procedure estimates the q-values for a given set of p-values. The q-value of a test measures the
#' proportion of false positive incurred when that particular test is called sigificant.
#' It gives the scientist a hypothesis testing error measure for each observed statistic with respect to the pFDR.
#' 
#' Note: If no options are selected, then the method used to estimate pi0 is the smoother method desribed in Storey and Tibshirani (2003). 
#' The bootstrap method is described in Storey, Taylor and Siegmund (2004). 
#' 
#' @param pValues pValues to be used (only necessary input)
#' @param lambda Value of the tuning parameter to be used
#' @param pi0.method Method for automatically choosing tuning parameter in the estimation of pi_0. Either 'smoother' or 'bootstrap'
#' @param fdr.level Level at which to control the FDR
#' @param robust Logical, whether to make estimate more robust for small p-values. 
#' @param smooth.df Number of degrees of freedom to use when estimating pi_0 with the smoother.
#' @param smooth.log.pi0 Logical, if TRUE and pi0.method = 'smoother', pi0 will be estimated by applying a smoother 
#'  		to a scatterplot of log(pi_0) estimates against the tuning parameter lambda.
#' @param silent 
#' @return A list containing:
#' 
#'	\item{qValues}{A vector of the estimated q-values}
#' 
#'	\item{pi0}{An estimate of the proportion of null hypotheses} 
#' 
#' 	\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function.}
#' 
#' @author HackNiklas
#' 
#' @references Storey, John (2001). The Positive False Discovery Rate: A Baysian Interpretation and the Q-Value.
#' The Annals of Statistics, Vol. 31, No. 6, 2013-2035.
#' @export
#' @examples 
#' pval <- c(runif(50), runif(50, 0, 0.01))
#' result <- Qvalue(pval)
#' result <- Qvalue(pval, lambda=0.5)
Qvalue <- function(pValues,lambda=seq(0,.90,.05),pi0.method="smoother", fdr.level=NULL,robust=FALSE, smooth.df=3,smooth.log.pi0=FALSE, silent=FALSE) {
	require(qvalue)
	out<-qvalue(pValues,lambda,pi0.method, fdr.level,robust, gui=FALSE,smooth.df,smooth.log.pi0)
	qValues<-out$qvalues
	pi0<-out$pi0
	if (! silent)
	{
		cat("\n\n\t\tStorey's (2001) q-value Procedure\n\n")
		cat("Number of hyp.:\t", length(pValues), "\n")
		cat("Estimate of the prop. of null hypotheses:\t", pi0, "\n")
	}
	return(list(qValues=qValues,pi0=pi0,errorControl = new(Class='ErrorControl',type="pFDR")))
}
#' @export
mutoss.Qvalue <- function() { return(new(Class="MutossMethod",
					label="Storey's (2001) q-value Procedure",
					errorControl="pFDR",
					callFunction="Qvalue",
					output=c("qValues", "pi0", "errorControl"),
					info="<h2>Storey (2001) qvalue Procedure </h2>\n\n\
							<h3>Reference:</h3>\
							<ul>\
							<li>Storey, John \"<i> The Positive False Discovery Rate: A Baysian Interpretation and the Q-Value.
							</i>\" The Annals of Statistics 2001, Vol. 31, No. 6, 2013-2035, 2001. </li>\n\
							</ul>
							<p>The Qvalue procedure estimates the q-values for a given set of p-values. The q-value of a test measures the \
							proportion of false positive incurred when that particular test is called sigificant. \
							It gives the scientist a hypothesis testing error measure for each observed statistic with respect to the pFDR. \
							Note: If no options are selected, then the method used to estimate pi0 is the smoother method desribed in Storey and Tibshirani (2003). \
							The bootstrap method is described in Storey, Taylor and Siegmund (2004). </p>\n",
					parameters=list(pValues=list(type="numeric"), lambda=list(type="numeric",optional=TRUE,label="Tuning parameter lambda"), pi0.method=list(type="character",optional=TRUE,choices=c("smoother","bootstrap"),label="Tuning parameter for the estimation of pi_0"),
							fdr.level=list(type="numeric",optional=TRUE,label="Level at which to control the FDR"),robust=list(type="logical",optional=TRUE,label="Robust estimate"),
							smooth.df=list(type="integer",optional=TRUE,label="Number of degrees-of-freedom"),smooth.log.pi0=list(type="logical",optional=TRUE))
			)) }






#' Blanchard-Roquain (2008) step-up Procedure for arbitrary dependent p-Values
#' Also proposed independently by Sarkar (2008)
#' 
#' A generalization of the Benjamini-Yekutieli procedure, taking as an additional parameter
#' a distribution pii on [1..k] (k is the number of hypotheses)
#' representing prior belief on the number of hypotheses that will be rejected.
#' 
#' It is a step-up Procedure with critical values C_i defined as alpha/k times
#' the sum for j in [1..i] of j*pii[j]. For any fixed prior pii, the FDR is controlled at
#' level alpha for arbitrary dependence structure of the p-Values. The particular case of the
#' Benjamini-Yekutieli step-up is recovered by taking pii[i] proportional to 1/i.
#' 
#' If pii is missing, a default prior distribution proportional to exp( -i/(0.15*k) ) is taken.
#' It should perform better than the BY procedure if more than about 0.05 to 0.1 of hypotheses are rejected,
#' and worse otherwise.
#' 
#' Note: the procedure automatically normalizes the prior pii to sum to one if this is not the case.
#' 
#' 
#' @param pValues pValues to be used. They can have arbitrary dependence. 
#' @param alpha the level at which the FDR should be controlled
#' @param pii Prior for the proportion of true null hypotheses, same size as pValues
#' @param silent if true any output on the console will be suppressed.  
#' @return A list containing:
#' 
#'	\item{adjPValues}{A numeric vector containing the adjusted pValues}
#' 
#'	\item{rejected}{A logical vector indicating which hypotheses are rejected}
#' 
#'	\item{criticalValues}{A numeric vector containing critical values used in the step-up test} 
#' 
#' 	\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}
#' 
#' @author GillesBlanchard,HackNiklas
#' @references Blanchard, G. and Roquain, E. (2008). Two simple sufficient conditions for FDR control.
#'             Electronic Journal of Statistics, 2:963-992. 
#' 			   Sarkar, S.K. (2008) On methods controlling the false discovery rate. 
#'             Sankhya, Series A, 70:135-168.
#' @export
BlaRoq<-function(pValues, alpha, pii, silent=FALSE){
	k <- length(pValues)
	
	if (missing(pii)) {
		pii = sapply( 1:k, function(i) exp(-i/(0.15*k)) ) # a default choice different from BY, exponential decreasing prior
	}
	
	if (any(pii<0)) {
		stop("BlaRoq(): Prior pii can only have positive elements")
	}
	
	if ( length(pii) != k) {
		stop("BlaRoq(): Prior pii must have the same length as pValues")
	}
	
	pii <- pii / sum(pii)
	
	precriticalValues <- cumsum(sapply(1:k, function(i) (i*pii[i])/k))
	# The following code is inspired from  p.adjust	
	i <- k:1
	o <- order(pValues, decreasing = TRUE)
	ro <- order(o)
	adjPValues <- pmin(1, cummin( pValues[o] / precriticalValues[i] ))[ro]
	
	rejected <- (adjPValues <= alpha)
	if (! silent)
	{
		cat("\n\n\t\t Blanchard-Roquain/Sarkar (2008) step-up for arbitrary dependence\n\n")
		printRejected(rejected, pValues, adjPValues)
	}
	return(list(adjPValues=adjPValues, criticalValues=alpha*precriticalValues, rejected=rejected,
					errorControl = new(Class='ErrorControl',type="FDR",alpha=alpha)))
}


#' @export
mutoss.BlaRoq <- function() { return(new(Class="MutossMethod",
					label="Blanchard-Roquain/Sarkar (2008) step-up",
					errorControl="FDR",
					callFunction="BlaRoq",
					output=c("adjPValues", "criticalValues", "rejected", "errorControl"),
					info="<h2>Blanchard-Roquain (2008) step-up Procedure for arbitrary dependent p-Values.</h2>\n\
							(Also proposed independently by Sarkar (2008))\n\
							<h3>References:</h3>\
							<ul>\
							<li>Blanchard, G. and Roquain, E. (2008).\"<i> Two simple sufficient conditions for FDR control.\
							</i>\" Electronic Journal of Statistics, 2:963-992. </li>\n\
							<li>Sarkar, S. K. (2008) \"<i>On methods controlling the false discovery rate.</i>\"\ 
							Sankhya, Series A, 70:135-168</li>\n\
							</ul>\
							<p>A generalization of the Benjamini-Yekutieli procedure, taking as an additional parameter
							a distribution pi on [1..k] (k is the number of hypotheses)
							representing prior belief on the number of hypotheses that will be rejected.</p>
							<p>The procedure is a step-up with critical values C_i defined as alpha/k times
							the sum for j in [1..i] of j*pi[j]. For any fixed prior pii, the FDR is controlled at
							level alpha for arbitrary dependence structure of the p-Values. The particular case of the
							Benjamini-Yekutieli step-up is recovered by taking pii[i] proportional to 1/i.
							If pii is missing, a default prior distribution proportional to exp( -i/(0.15*k) ) is taken.
							It should perform better than the BY procedure if more than about 5% to 10% of hypotheses are rejected,
                            and worse otherwise.
							</p>
							
							<p>Note: the procedure automatically normalizes the prior pii to sum to one if this is not the case.
							</p>\n",
					parameters=list(pValues=list(type="numeric"), alpha=list(type="numeric"), pii=list(type="RObject",label="Prior pi",optional=TRUE))
			)) }











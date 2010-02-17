# 
# 
# Author: FrankKonietschke
###############################################################################



#' Xin Gao's non-parametric multiple test procedure is applied to Data. 
#' The procedure controls the FWER in the strong sense. Here, only the Many-To-One comparisons are computed.
#' 
#' This function computes Xin Gao's nonparametric multiple test procedures in an unbalanced one way layout. 
#' It is based upon the following purely nonparametric effects: 
#' Let \eqn{F_i} denote the distribution function of sample \eqn{i, i=1,\ldots,a,} and let \eqn{G} denote the mean distribution function
#' of all distribution functions \eqn{(G=1/a\sum_i F_i)}. The effects \eqn{p_i=\int GdF_i} are called unweighted relative effects. If \eqn{p_i>1/2}, the random
#' variables from sample \eqn{i} tend (stochastically) to larger values than any randomly chosen number 
#' from the whole experiment. If \eqn{p_i = 1/2}, there is no tendency to smaller nor larger values. However,
#' this approach tests the hypothesis \eqn{H_0^F: F_1=F_j, j=2,\ldots,a} formulated in terms of the
#' distribution functions, simultaneously. 
#'  
#' @param formula Formula defining the statistical model, containing the response and the factors
#' @param data Dataset containing the response and the grouping factor
#' @param alpha The level at which the FWER shall be controlled. By default it is alpha=0.05.
#' @param silent If true any output on the console will be suppressed.  
#' @param control The control group for the Many-To-One comparisons. By default it is the first group in lexicographical order. 
#' @return A list containing:
#' 
#'	\item{adjPValues}{A numeric vector containing the adjusted pValues}
#' 
#'	\item{rejected}{A logical vector indicating which hypotheses are rejected}
#' 
#'  \item{confIntervals}{A matrix containing the estimates and the lower and upper confidence bound}
#' 
#' 	\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}
#' 
#' @author Frank Konietschke
#' @references 	Gao, X. et al. (2008). Nonparametric multiple comparison procedures for unbalanced
#' one-way factorial designs. 
#' Journal of Statistical Planning and Inference 77, 2574-2591. \eqn{n}
#' The FWER is controlled by using the Hochberg adjustment
#' (Hochberg, Y. (1988). A sharper Bonferroni procedure for multiple tests of significance. Biometrika 75, 800-802.)
#' @export
#' @examples 
#' x=c(rnorm(40))
#' f1=c(rep(1,10),rep(2,10),rep(3,10),rep(4,10))
#' my.data <- data.frame(x,f1)
#' result <- gao(x~f1,data=my.data, alpha=0.05,control=2, silent=FALSE)
#' result <- gao(x~f1,data=my.data, alpha=0.05,control=2, silent=TRUE)
#' result <- gao(x~f1,data=my.data, alpha=0.05)
gao<-function(formula, data, alpha = 0.05, control = NULL , silent = FALSE){
	
requireLibrary("multcomp")
	
	
	dat <- model.frame(formula, data)
	if (ncol(dat) != 2) {
		stop("Specify one response and only one class variable in the formula !")
	}
	if (is.numeric(dat[, 1]) == FALSE) {
		stop("Response variable must be numeric !")
	}
	response <- dat[, 1]
	group <- as.factor(dat[, 2])
	fl <- levels(group)
	a <- nlevels(group)
	N <- length(response)
	n <- aggregate(response,list(group),FUN="length")$x
	
	if (any(n <= 2)) {
		warn <- paste("The factor level", fl[n <= 2], "has got less than two observations!")
		stop(warn)
	}
	if (is.null(control)) {
		cont <- 1
	
	}

	if(! is.null(control)){
   if (!any(fl == control)) {
		stop("The dataset doesn't contain this control group!")
	}
	cont <- which(fl == control)
}
	
	
	C<-contrMat(1:a,"Dunnett",base=cont)
	
	
	
	
# ------------- Compute the pseudo-ranks------------------ #
	
	
	#browser()
	rx <- c()
	
	for (i in 1:N){
		
		help <- expand.grid(response[i],response)
		
		help1 <- (help[,1]>help[,2])+1/2*(help[,1]== help[,2])
		help2 <- data.frame(h1=help1,h2=group)
		samples2 <- split(help2$h1, help2$h2)
		
		pseudo <- sapply(1:a, function(arg) {
					
					mean(samples2[[arg]])
				})
		
		rx[i] <-N*mean(pseudo)
	}
	new.data <-data.frame(res=rx,group=group)
	
	
# ------------------ Point estimators ---------------------#
	
	pd <- 1/N*aggregate(new.data$res,list(group), FUN="mean")$x
	
	Cpd <- C%*%pd
	
# ------------ Compute the variance estimators ----------- #
	
	v1 <- 1/N^2*aggregate(new.data$res,list(group),FUN="var")$x
	lambda <- N/n
	v11 <-c(v1*lambda)
	v2 <- diag(v1*lambda)
	
	Cv <- C%*%v2%*%t(C)
	
	
# ------------------ Test Statistics ----------------------#
	
	T <-sqrt(N)*Cpd / sqrt(c(diag(Cv))) 
	

# ------------------ Degrees of freedom--------------------#
	
	
	ncont <-which((1:a)!= cont)
	
	numerator <- c(diag(Cv))^2
	denu1<-v1[cont]^2/(n[cont]^2*(n[cont]-1))
	denu2 <- v1[ncont]^2 /(n[ncont]^2*(n[ncont]-1))
	denu <- N^2*(denu1 + denu2)
	
	df <- numerator / denu
	
	
	
#-------------------------p-Values ------------------------#
	
	pv<- c()
	
	for (h in 1:(a-1)){
		pv[h]<- min(2*pt(T[h],df[h]),2-2*pt(T[h],df[h]))
	}
	
	adj.p <- p.adjust(pv,"hochberg")
	Rejected <- (adj.p<=alpha)
	
#------------------- Build the output ---------------------# 
	vj <- which((1:a) != cont)
	vi <- rep(cont, a - 1)
	cmpid <- sapply(1:(a-1), function(arg) {
				i <- vi[arg]
				j <- vj[arg]
				paste("F", "(", fl[j], ")", "-","F","(" ,fl[i],")", sep = "")
			})

	
	result <- data.frame(Comparison=cmpid, Estimator = Cpd, df=df, Statistic = T, P.Raw=pv,P.Adj=adj.p,Rejected = Rejected )
	rownames(result)<-1:(a-1)
	
	
	output = list(Info=data.frame(Sample=fl, Size=n, Single.Effects=pd),
			Analysis=result)
	if (! silent)
	{
		cat("#----Xin Gao's (2008) Non-Parametric Multiple Test Procedure","\n") 
		cat("#----Type of Adjustment: Hochberg", "\n")
		cat("#----Level of significance", "=", alpha ,"\n")
		cat("#----The procedure compares if the distribution functions F() are equal. The FWER is strongly controlled", "\n")
		print(result)
	}
	return(output)
	
}


gao.wrapper <- function(model, data,  alpha, control) {
	control <- NULL
	result <- gao(formula=formula(model), 
			data=data, 
			alpha = alpha,control)
	
	
	

	
	pvalues <- result$Analysis$P.Adj
	estimates <- result$Analysis$Estimator
	confint <- cbind(estimates, rep(NA, length(estimates)),rep(NA,length(estimates)))
	rownames(confint)<-result$Analysis$Comparison
	rejected1 <- result$Analysis$Rejected

	
	return(list(adjPValues=pvalues,rejected=rejected1,confIntervals= confint,
					errorControl = new(Class='ErrorControl',type="FWER",alpha=alpha)))
}


mutoss.gao <- function() { return(new(Class="MutossMethod",
					label="Nonparametric Multiple contrast tests",
					errorControl="FWER",
					callFunction="gao.wrapper",
					output=c("adjPValues", "rejected","confIntervals","errorControl"),
					info="<h2>Nonparametric multiple contrast tests</h2>
							<p> This function computes Xin Gao's nonparametric multiple test procedures in an unbalanced one way layout. <p> 
							<p></p>
							<h3>Reference:</h3>
							<ul>
							<li>Gao, X. et al. \"<i>Nonparametric multiple comparison procedures for unbalanced one-way factorial designs.</i>\" Journal of Statistical Planning and Inference, 77, 2574-2591, 2008.</li>
							</ul>",
					parameters=list(model=list(type="ANY"),
							hypotheses=list(type="ANY"),
							alpha=list(type="numeric")
							
							
					
					)
			)) }



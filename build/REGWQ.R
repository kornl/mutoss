# REGWQ - Ryan / Einot and Gabriel / Welsch  test procedure
# 
# Author: FrankKonietschke
###############################################################################
#' REGWQ - Ryan / Einot and Gabriel / Welsch  test procedure
#' This function computes REGWQ test for given
#' data including p samples. It is based on a stepwise or 
#' layer approach to significance testing. Sample means are 
#' ordered from the smallest to the largest.  
#'  The largest 
#' difference, which involves means that are r = p steps apart, 
#' is tested first at $\alpha$ level of significance; if significant, 
#' means that are $r <p$ steps apart are tested at a different $\alpha$ level 
#' of significance and so on. Compare to the Student-
#' Newman-Keuls test, the $\alpha$ levels are adjusted for the p-1 different
#' layers by the formula $\alpha_p=\alpha$, if p=k or p=k-1,
#' $\alpha_p = 1-(1-\alpha)^{p/k}$  otherwise. It might happen that the
#' quantiles are not descending in p. In this case, they are adapted by
#' $c_k = max_{2\leq r \leq k} c_r, k=2,\ldots,p$.
#' The REGWQ procedure, like Tukey's procedure, requires 
#' equal sample n's. However, in this algorithm, the procedure is 
#' adapted to unequal sample sized which can lead to still 
#' conservative test decisions. 
#'  
#' @param formula Formula defining the statistical model containing the response and the factors
#' @param data dataset containing the response and the grouping factor  
#' @param alpha  The level at which the error should be controlled. By default it is alpha=0.05.
#' @param MSE Optional for a given variance of the data
#' @param df Optional for a given degree of freedom 
#' @param silent If true any output on the console will be suppressed.  
#' @return List with the the adjusted pValues, rejected, indicating which hypotheses are rejected,
#' the test-statistics and the estimates for the different parameters.  
#'	\item{adjPValues}{A numeric vector containing the adjusted pValues}
#' 
#'	\item{rejected}{A logical vector indicating which hypotheses are rejected}
#' 
#'	\item{statistics}{A numeric vector containing the test-statistics}
#' 
#'  \item{confIntervals}{A matrix containing only the estimates}
#' 
#'  \item{errorControl}{A class of type \code{errorControl} containing the error control and alpha} 
#' @author Frank Konietschke
#' @references Hochberg, Y. & Tamhane, A. C. (1987). Multiple Comparison Procedures,  Wiley. 
#' @export
#' @examples 
#' x = rnorm(50)
#' grp = c(rep(1:5,10))
#' dataframe <- data.frame(x,grp)
#' result <- regwq(x~grp, data=dataframe, alpha=0.05,MSE=NULL, df=NULL, silent = TRUE)
#' result <- regwq(x~grp, data=dataframe, alpha=0.05,MSE=NULL, df=NULL, silent = FALSE)
#' result <- regwq(x~grp, data=dataframe, alpha=0.05,MSE=1, df=Inf, silent = FALSE) # known variance
#' result <- regwq(x~grp, data=dataframe, alpha=0.05,MSE=1, df=1000, silent = FALSE) # known variance




regwq <- function(formula, data,alpha, MSE=NULL, df=NULL, silent = FALSE){
	
	dat <- model.frame(formula, data)
	if (ncol(dat) != 2) {
		stop("Specify one response and only one class variable in the formula")
	}
	if (is.numeric(dat[, 1]) == FALSE) {
		stop("Response variable must be numeric")
	}
	response <- dat[, 1]
	group <- as.factor(dat[, 2])
	fl <- levels(group)
	a <-nlevels(group)
	N <- length(response)
	samples <- split(response,group)
	n <- sapply(samples,"length")
	mm <- sapply(samples,"mean")
	vv <- sapply(samples,"var")
	if (is.null(MSE)){
		MSE <- sum((n-1)*vv)/(N-a)
	}
	if (is.null(df)){
		df <- N-a
	}
	
	nc <- a*(a-1)/2
	order.h1 <- data.frame(Sample=fl, Size=n, Means=mm,Variance=vv)
	
	ordered <- order.h1[order(order.h1$Means,decreasing=FALSE), ]
	rownames(ordered) <- 1:a
	
	
	
	#---------------- Compute helping indices ----------#
	
	i <- 1:(a-1)
	
	h1 <- list()
	for(s in 1:(a-1)){
		
		h1[[s]]<- i[1:s]
		
	}
	vi <- unlist(h1)
	
	j <- a:2
	h2 <-list()
	
	for (s in 1:(a-1)){
		h2[[s]] <- j[s:1]
	}
	
	vj <- unlist(h2)
	
	h3 <- list()
	h4 <- list()
	
	for (s in 1:(a-1)){
		h3[[s]] <- rep(j[s],s)
		h4[[s]] <- rep(i[s],s)
	}
	Nmean <- unlist(h3)
	Step  <- unlist(h4)
	
	
	#--------Compute the Mean Differences---------#
	
	mean.difference <- sapply(1:nc,function(arg){
				i <- vi[arg]
				j <- vj[arg]
				(ordered$Means[j]-ordered$Means[i])
				
			})
	mean.difference <- round(mean.difference, 4)
	
	# ------- Compute the test statistics --------#
	
	T <- sapply(1:nc,function(arg){
				i<-vi[arg]
				j<-vj[arg]
				(ordered$Means[j]-ordered$Means[i])/sqrt(MSE/2*(1/ordered$Size[i] + 1/ordered$Size[j]))
				
			})
	
	T <- round(T, 4)
	#-------Compute the adjusted p-Values-------#
    pvalues <- ptukey(T,Nmean,df,lower.tail=FALSE)
	
	#------Compute the adjusted alpha-levels----#

    alpha.level <- 1-(1-alpha)^(Nmean/a)
	level1 <- (Nmean==a)
	level2 <- (Nmean==a-1)
	level3 <- level1 + level2
	alpha.level[level3==1] <- alpha
	alpha.level <- round(alpha.level,4)
	


	# ----- Compute now the critical value -----#
	
	quantiles <- qtukey(1-alpha.level,Nmean,df)
	
	for (h in 1:(nc-1)){
		if (quantiles[h+1] >=quantiles[h]){
			quantiles[h+1] <- quantiles[h]
		}
		
	}
	

	#---- Calculate the rejected Hypotheses ------# 
	
	Rejected1 <- (pvalues<alpha.level)
	
	#------------ Names for the Output -----------# 
	
	names.ordered <- sapply(1:nc, function(arg){
				i <- vi[arg]
				j <- vj[arg]
				paste(ordered$Sample[j], "-", ordered$Sample[i], sep="")
			})
	
	# ------ Compute now the rejected statistics-----#
	
	for (s in 1:nc){
		if (Rejected1[s]==FALSE){
			Under1 <- (vj[s]>=vj)
			Under2 <- (vi[s]<=vi)
			Under3 <- Under1 * Under2
			Under4 <- which(Under3==1)
			
			Rejected1[Under4] <- FALSE
			
		}
	}
	
	
	#-----Prepare the pValues for the Output----#
	
	Out1 <- (pvalues < alpha.level)
	Out2 <- (Rejected1 == FALSE)
	Out3 <- Out1 * Out2
	Out4 <- (Out3 == 1)
	pvalues <- round(pvalues,4)
	quantiles <- round(quantiles,4)
	pvalues[Out4] <- paste(">",alpha.level[Out4])
	quantiles[Out4] <- paste(">", T[Out4])
	variances.output <- data.frame(Overall=MSE, df=df)
	Comparison <- data.frame(Comparison=names.ordered,Diff=mean.difference, Statistic=T,Quantiles=quantiles,  Adj.P=pvalues, Alpha.Level=alpha.level, Rejected=Rejected1, Layer = Step)
	
	
	if (!silent)
	{
		cat("#----REGWQ - Ryan / Einot and Gabriel / Welsch  test procedure  \n\n")
		printRejected(Comparison$Rejected, pvalues, Comparison$Adj.P)
	}
	
	
	#result <- list(Ordered.Means = ordered, Variances=variances.output,
	#		REGWQ = Comparison)
	
	#diffm<-matrix(c(Comparison["Diff"],rep(NA,length(Comparison["Diff"])*2)),nrow=length(Comparison["Diff"]))
	diffm<-cbind(Comparison$Diff,rep(NA,length(Comparison$Diff)),rep(NA,length(Comparison$Diff)))	
	diffm<-matrix(diffm,nrow=length(Comparison$Diff))
	
	rownames(diffm)<-Comparison$Comparison
	
	return(list(adjPValues=Comparison$Adj.P, rejected=Comparison$Rejected, statistic=Comparison$Statistic,
					confIntervals=diffm,errorControl = new(Class='ErrorControl',type="FWER",alpha=alpha)))	
}


#' @export 
mutoss.regwq <- function() { return(new(Class="MutossMethod",
					label="Ryan / Einot and Gabriel / Welsch  test",
					errorControl="FWER",
					callFunction="regwq",
					output=c("adjPValues","rejected","statistic","confIntervals","errorControl"),
					info="<h2>Ryan / Einot and Gabriel / Welsch  test procedure.
							The procedure controls the FWER. </h2>\n\n\
							<p> It is based on a stepwise or \
							layer approach to significance testing. Sample means are \
							ordered from the smallest to the largest. The largest \
							difference, which involves means that are r = p steps apart, \
							is tested first at $\alpha$ level of significance; if significant, \
							means that are $r < p$  steps apart are tested at an adjusted $\alpha$ level \
							of significance and so on. \ 
                            The $\alpha$ levels are adjusted for the p-1 different\
                            layers by the formula $\alpha_p=\alpha$, if p=k or p=k-1,\
                            $\alpha_p = 1-(1-\alpha)^{p/k}$  otherwise.
							</p>\n\
							<h3>Reference:</h3>\
							<ul>\
							<li>Hochberg, Y., Yamhane, A.C. (1987).  \"<i> Multiple Comparison Procedures \
							
							</i>\" Wiley, New York. </li>\n\
							</ul>",
					parameters=list(formula=list(type="formula"), data=list(data.frame), alpha=list(type="numeric"), 
							MSE=list(type="numeric"), df=list(type="numeric"))
			)
	) }




	
	



# 
# Author: JonathanRosenblatt
###############################################################################


#----------Unused Adaptive BH :-( ------------#

#' Estimate m0 using for the adaptive BH procedure
#' 
#' @param sorted Sorted p-values.
#' @param m Number of hypotheses.
#' @return \item{m0}{The estimated number of true null hypotheses} 
#' @author JonathanRosenblatt
#' @export
bh.m0.estimate<- function(sorted, m){
	k<- 2:m
	m0.k<- (m+1-k)/(1-sorted[-1])
	diffs<- diff(m0.k, lag=1)
	indicators<-  diffs>0
	optimal.k<- ifelse(any(indicators) , min( k[indicators] ) , 1)
	m0<- min(ceiling(m0.k[optimal.k]), m)
	stopifnot(m0 <=m ,m0 > 0)
	return(m0)
}

#' Adaptive Benjamini Hochberg (2000)
#' 
#' Applies the Benjamini Hochberg procedure to a vector of sortev pvalues returning the adjusted p-values, critical values, and rejections.
#' @param pValues 
#' @param alpha 
#' @returnType 
#' @return 
#' @author JonathanRosenblatt
#' @export
#' @references Benjamini, Y. and Hochberg, Y. (2000). On the Adaptive Control of the False Discovery Rate in Multiple Testing With Independent Statistics
adaptive.bh<- function(pValues, alpha){
	m<- length(pValues)
	ranks<- rank(pValues)
	sorted<-sort(pValues)
	
	#Stage I- estimating m0
	stage.one<- bh(sorted, alpha, adjust=TRUE,m=m)
	r<- sum(stage.one$Pvals[['rejected']])
	#Anything to reject?                                          
	if( r==0 ) return(stage.one)  
	#Stage II- estimate m0
	else m0<- bh.m0.estimate(sorted=sorted, m=m)  
	
	output=bh(sorted=sorted,q=alpha,m0=m0,m=m)  
	output$Pvals[['adjusted.pvals']]=bh.adjust(sorted,m=m,m0=m0)
	
	output$Pvals=output$Pvals[ranks,]
	output.2=list(
			criticalValues=output[['Pvals']]$criticals,
			rejected=output[['Pvals']]$rejected,
			adjPValues=output[['Pvals']]$adjusted.pvals,
			errorControl=new(Class='ErrorControl',type="FDR",alpha=alpha),
			pi0=
	)
	return(output.2)	
}

#pvals<- runif(100)^2
#adaptive.bh(pvals, 0.1)
#plot(pvals~adaptive.bh(pvals,0.1)[['criticalValues']])
#plot(pvals~adaptive.bh(pvals,0.1)[['adjPValues']])
#plot(pvals~adaptive.bh(pvals,0.1)[['rejected']])

mutoss.adaptive.bh <- function() { return(new(Class="MutossMethod",
					label="Benjamini-Hochberg (2000) Adaptive step-up",
					errorControl="FDR",
					callFunction="adaptive.bh",
					output=c("adjPValues", "criticalValues", "rejected", "pi0", "errorControl"),
					assumptions=c("independence or positive regression dependency"),
					info="<h2>Benjamini-Hochberg (2000) adaptive linear step-up procedure </h2>\n\n\
							<h3>Reference:</h3>\
							<ul>\
							<li>Benjamini, Y. and Hochberg, Y. (2000). <i> On the Adaptive Control of the False Discovery Rate
							in Multiple Testing With Independent Statistics. </i> Journal of Educational and Behavioral Statistics, 25(1): 60-83. </li>\n\
							<li>Benjamini, Y. and Hochberg, Y. (1995). <i> Controlling the false discovery rate: A practical and powerful approach to mulitple testing.
							</i>Journal of the Royal Statistical Society, Series B, 57:289-300. </li>\n\
							<li> Hochberg, Y. and Benjamini, Y. (1990). <i>More powerful procedures for multiple significance testing. </i>
							Statistics in Medicine 9, 811-818.</li>\n
							</ul>
							<p>The adaptive Benjamini-Hochberg step-up procedure is applied to pValues.
							It controls the FDR at level alpha for independent or positive regression dependent test statistics.
							In the adaptive Benjamini-Hochberg step-up procedure the number of true null hypotheses is estimated first as in Hochberg and
							Benjamini (1990), and this estimate is used in the procedure of Benjamini and
							Hochberg (1995) with alpha'=alpha*m/m0. The method for estimating m<sub>0</sub> is motivated by 
							the graphical approach proposed by Schweder and Spjotvoll (1982), 
							as developed and presented in Hochberg and Benjamini (1990).</p>\n",
					parameters=list(pValues=list(type="numeric"), alpha=list(type="numeric"))
			)) }

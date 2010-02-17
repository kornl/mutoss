# Here Benjamini-Liu, Benjamini-Hochberg, Benjamini-Yekutieli , Hochberg
# Hommel can be found.
# 
# Author: WerftWiebke
###############################################################################



#' Benjamini-Liu (1999) step-down procedure
#' @param pValues 
#' @param alpha 
#' @return blabla
#' @author WerftWiebke
#' @export
BL <- function(pValues, alpha) {
	m <- length(pValues)
	criticalValues <- sapply(1:m, function(i) 1-(1-min(1,(m*alpha)/(m-i+1)))^(1/(m-i+1)))
	rejected <- SD(pValues, criticalValues)
	index <- order(pValues)
	spval <- pValues[index]
	adjPValues <- vector(mode="numeric",length=m)
	adjPValues[1] <- min(1 - (1 - spval[1])^m, 1)
	for (i in 2:m) adjP[i] <- max(adjPValues[i - 1], ifelse((alpha*m)/(m-i+1)<=1,((m-i+1)/m)*(1 - (1 - spval[i])^(m - i + 1)),0))
	adjpPValues[index] <- adjPValues
	#rejected <- (adjustedPValues <= alpha)
	if (! silent)
	{
		cat("\n\n\t\tBenjamini-Liu's (1990) step-down Procedure\n\n")
		printRejected(rejected, pValues, NULL)
	}
	return(list(adjPValues=adjPValues, criticalValues=criticalValues, rejected=rejected))
}
#' @export
mutoss.BL <- function() { return(new(Class="MutossMethod",
					label="Benjamini-Liu (1999) step-down",
					errorControl="FDR",
					callFunction="BL",
					output=c("adjPValues", "criticalValues", "rejected"),
					assumptions=c("test independence"),
					info="<h2>Benjamini-Liu (1999) step-down procedure </h2>\n\n\
							<h3>Reference:</h3>\
							<ul>\
							<li>Bejamini, Yoav and Liu, Wei. \"<i> A step-down multiple hypotheses testing procedure that controls the false discovery rate under independence . </i>\" Journal of Statistical Planning and Inference Vol. 82(1-2): 163-170. </li>\n\
							</ul>
							<p>This method assumes independence of the tests. It is most suitable when the number of hypotheses is small and if most null hypotheses are far from being true then this procedure should be more powerful than Benjamini-Hochberg step-up method.</p>\n",
					parameters=list(pValues=list(type="numeric"), alpha=list(type="numeric"))
			)) }




#' Benjamini-Hochberg (1995) linear step-up Procedure
#' @param pValues 
#' @param alpha 
#' @return blabla
#' @author WerftWiebke
#' @export
BH <- function(pValues, alpha) {
	m <- length(pValues)
	criticalValues <- sapply(1:m, function(i) (i*alpha)/m)
	#rejected <- SU(pValues, criticalValues)
	adjPValues <- p.adjust(pValues, "BH")
	rejected <- (adjustedPValues <= alpha)
	if (! silent)
	{
		cat("\n\n\t\tBenjamini-Hochberg's (1995) step-up Procedure\n\n")
		printRejected(rejected, pValues, NULL)
	}
	return(list(adjPValues=adjPValues, criticalValues=criticalValues, rejected=rejected))
}
#' @export
mutoss.BH <- function() { return(new(Class="MutossMethod",
					label="Benjamini-Hochberg (1995) step-up",
					errorControl="FDR",
					callFunction="BH",
					output=c("adjPValues", "criticalValues", "rejected"),
					assumptions=c("test independence or positive regression dependency"),
					info="<h2>Benjamini-Hochberg (1995) linear step-up Procedure </h2>\n\n\
							<h3>Reference:</h3>\
							<ul>\
							<li>Bejamini, Yoav and Hochberg, Josef. \"<i> Controlling the false discovery rate: A practical and powerful approach to mulitple testing. </i>\" Journal of the Royal Statistical Society, Series B, 57:289-300, 1995. </li>\n\
							</ul>
							<p>This method assumes independence of the tests or a certain form of positive regression dependency.</p>\n",
					parameters=list(pValues=list(type="numeric"), alpha=list(type="numeric"))
			)) }



#' Benjamini-Yekutieli (2001) step-up procedure
#' @param pValues 
#' @param alpha 
#' @return blabla
#' @author WerftWiebke
#' @export
BY <- function(pValues, alpha) {
	m <- length(pValues)
	a <- sum(1/(1:m))
	criticalValues <- sapply(1:m, function(i) (i*alpha)/(a*m))
	#rejected <- SU(pValues, criticalValues)
	adjPValues <- p.adjust(pValues, "BY")
	rejected <- (adjustedPValues <= alpha)
	if (! silent)
	{
		cat("\n\n\t\tBenjamini-Yekutieli's (1990) step-up Procedure\n\n")
		printRejected(rejected, pValues, NULL)
	}
	return(list(adjPValues=adjPValues, criticalValues=criticalValues, rejected=rejected))
}
#' @export
mutoss.BY <- function() { return(new(Class="MutossMethod",
					label="Benjamini-Yekutieli (2001) step-up",
					errorControl="FDR",
					callFunction="BY",
					output=c("adjPValues", "criticalValues", "rejected"),
					assumptions=c("any dependency structure"),
					info="<h2>Benjamini-Yekutieli (2001) step-up Procedure </h2>\n\n\
							<h3>Reference:</h3>\
							<ul>\
							<li>Bejamini, Yoav and Yekutieli, Daniel. \"<i> The control of the false discovery rate in multiple testing under dependency. </i>\" Annals of Statistics, 29(4):1165-1188, 2001. </li>\n\
							</ul>
							<p>This method assumes any dependency structure.</p>\n",
					parameters=list(pValues=list(type="numeric"), alpha=list(type="numeric"))
			)) }


#' Hochberg (1988) step-up procedure
#' @param pValues 
#' @param alpha 
#' @return blabla
#' @author WerftWiebke
#' @export
Hochberg <- function(pValues, alpha) {
	m <- length(pValues)
	criticalValues <- sapply(1:m, function(i) alpha/(m-i+1))
	#rejected <- SU(pValues, criticalValues)
	adjPValues <- p.adjust(pValues, "hochberg")
	rejected <- (adjustedPValues <= alpha)
	if (! silent)
	{
		cat("\n\n\t\tHochberg's (1988) step-up Procedure\n\n")
		printRejected(rejected, pValues, NULL)
	}
	return(list(adjPValues=adjPValues, criticalValues=criticalValues, rejected=rejected))
}
#' @export
mutoss.Hochberg <- function() { return(new(Class="MutossMethod",
					label="Hochberg (1988) step-up",
					errorControl="FWER",
					callFunction="Hochberg",
					output=c("adjPValues", "criticalValues", "rejected"),
					assumptions=c("test independence, "),
					info="<h2>Hochberg (1988) step-up procedure </h2>\n\n\
							<h3>Reference:</h3>\
							<ul>\
							<li>Hochberg, Josef. \"<i> A sharper Bonferroni procedure for multiple tests of significance. </i>\" Biometrika, 75:800-802, 1988. </li>\n\
							</ul>
							<p>Hochberg's method is more powerful than Holm's method, but the test statistics
								need to be independent or have a distribution with multivariate total positivity
								of order two or a scale mixture thereof for its validity (Sarkar, 1998)..</p>\n",
					parameters=list(pValues=list(type="numeric"), alpha=list(type="numeric"))
			)) }



#' adaptive Bejamini-Hochberg (2000) linear step-up Procedure
#' @param pValues 
#' @param alpha 
#' @return blabla
#' @author WerftWiebke
#' @export
adaptiveBH <- function(pValues, alpha) {
	m <- length(pValues)
	pi0.ABH <- ABH_pi0_est(pValues)
	criticalValues <- sapply(1:m, function(i) (i*alpha)/pi0.ABH)
	adjPValues <- p.adjust(pValues,"BH")*pi0.ABH/m
	rejected <- (adjPValues <= alpha)
		if (! silent)
	{
		cat("\n\n\t\tadaptive Benjamini-Hochberg's (2000) step-up Procedure\n\n")
		printRejected(rejected, pValues, adjPValues)
	}
	return(list(adjPValues=adjPValues, criticalValues=criticalValues, rejected=rejected, pi0=pi0.ABH))
}
#' @export
mutoss.adaptiveBH <- function() { return(new(Class="MutossMethod",
					label="adaptive Benjamini-Hochberg (2000) step-up",
					errorControl="FDR",
					callFunction="adaptiveBH",
					output=c("adjPValues", "criticalValues", "rejected", "pi0"),
					assumptions=c("test independence or positive regression dependency"),
					info="<h2>adaptive Benjamini-Hochberg (2001) linear step-up Procedure </h2>\n\n\
							<h3>Reference:</h3>\
							<ul>\
							<li>Bejamini, Yoav and Hochberg, Josef. \"<i> On the Adaptive Control of the False Discovery Rate
								in Multiple Testing With Independent Statistics. </i>\" Journal of Educational and Behavioral Statistics, 25(1): 60-83, 2000. </li>\n\
							</ul>
							<p>This methods revises the original Benjamini-Hochberg linear step up procedure by multiplying the adjusted pValues with an 
								estimate of the number of true null hypotheses pi0 divided by the total 
								number of hypotheses. The method for estimating pi0 is motivated by 
								the graphical approach proposed by Schweder and Spjotvoll (1982), 
								as developed and presented in Hochberg and Benjamini (1990).</p>\n",
					parameters=list(pValues=list(type="numeric"), alpha=list(type="numeric"))
			)) }


#' Benjamini-Krieger-Yekutieli (2006) two-stage step-up Procedure
#' @param pValues 
#' @param alpha 
#' @return blabla
#' @author WerftWiebke
#' @export
twostageBKY <- function(pValues, alpha, silent=FALSE) {
	m <- length(pValues)
	adjP <- p.adjust(pValues,"BH")
	pi0.TSBKY <- m - sum(adjp < alpha/(1 + alpha),	na.rm = TRUE)
	criticalValues <- sapply(1:m, function(i) (i*alpha)/pi0.TSBKY)
	adjPValues <- adjP*pi0.TSBKY/m
	rejected <- (adjPValues <= alpha)
	if (! silent)
	{
		cat("\n\n\t\tBenjamini-Krieger-Yekutieli (2006) two-stage step-up Procedure\n\n")
		printRejected(rejected, pValues, adjPValues)
	}
	return(list(adjPValues=adjPValues, rejected=rejected, pi0=pi0.TSBKY))
}
#' @export
mutoss.twostageBKY <- function() { return(new(Class="MutossMethod",
					label="Benjamini-Krieger-Yekutieli (2006) two-stage step-up",
					errorControl="FDR",
					callFunction="twostageBKY",
					output=c("adjPValues", "criticalValues", "rejected", "pi0"),
					assumptions=c("test independence or positive regression dependency"),
					info="<h2>Benjamini-Krieger-Yekutieli (2006) two-stage step-up Procedure</h2>\n\n\
							<h3>Reference:</h3>\
							<ul>\
							<li>Bejamini, Y., Krieger, A. and Hochberg, J. \"<i> Adaptive linear step-up procedures that control the false
								discovery rate. </i>\" Biometrika, 93(3):491–507, 2006. </li>\n\
							</ul>
							<p>The idea underlying the two-stage procedure is that the value of pi0 can be estimated from the
								results of applying the original Benjamini-Hochberg one-stage linear step-up procedure to <i>&alpha;/(1+&alpha;)</i>. 
								Then use the number rejected hypotheses <i>r1</i> at the first stage to estimate <i>pi0=m-r1</i>.
								 In stage two the original Benjamini-Hochberg linear step-up procedure is applied to <i>&alpha;'=&alpha;*pi0/m</i>. 
								The critical values are provided for the second stage and are dependent on the first stage through the choice of <i>&alpha;</i>.</p>\n",
					parameters=list(pValues=list(type="numeric"), alpha=list(type="numeric"))
			)) }



#' Storey-Taylor-Siegmund (2004) adaptive step-up Procedure
#' @param pValues 
#' @param alpha 
#' @return blabla
#' @author WerftWiebke
#' @export
adaptiveTST <- function(pValues, alpha, lambda, silent=FALSE) {
	m <- length(pValues)
	adjP <- p.adjust(pValues,"BH")
	pi0 <- storey_pi0_est(pValues, lambda)
	criticalValues <- sapply(1:m, function(i) (i*alpha)/(m*pi0))
	adjPValues <- adjP*pi0
	rejected <- (adjPValues <= alpha)
	if (! silent)
	{
		cat("\n\n\t\tStorey-Taylor-Siegmund (2004) adaptive step-up Procedure\n\n")
		printRejected(rejected, pValues, adjPValues)
	}
	return(list(adjPValues=adjPValues, criticalValues=criticalValues, rejected=rejected, pi0=pi0))
}
#' @export
mutoss.adaptiveTST <- function() { return(new(Class="MutossMethod",
					label="Storey-Taylor-Siegmund (2004) adaptive step-up",
					errorControl="FDR",
					callFunction="adaptiveTST",
					output=c("adjPValues", "criticalValues", "rejected", "pi0"),
					assumptions=c("test independence or positive regression dependency"),
					info="<h2>Storey-Taylor-Siegmund (2004) adaptive step-up Procedure</h2>\n\n\
							<h3>Reference:</h3>\
							<ul>\
							<li>Storey, J.D., Taylor, J.E. and Siegmund, D. \"<i> Strong control, conservative point estimation and
							simultaneous conservative consistency of false discovery rates: a unified approach </i>\" Journal of the Royal Statistical Society, B 66(1):187–205, 2004. </li>\n\
							</ul>
							<p>  </p>\n",
					parameters=list(pValues=list(type="numeric"), alpha=list(type="numeric"))
			)) }



#' Šidák-like (1987) step-down procedure
#' @param pValues 
#' @param alpha 
#' @return blabla
#' @author WerftWiebke
#' @export
SidakSD <- function(pValues, alpha, silent=FALSE) {
	m <- length(pValues)
	criticalValues <- sapply(1:m, function(i) 1-(1-alpha)^(1/(m-i+1)))
	#rejected <- SD(pValues, criticalValues)
	tmp <- mt.rawp2adjp(pValues, "SidakSD")
	adjPValues <- tmp$adjp[order(tmp$index),2]
	rejected <- (adjPValues <= alpha)
	if (! silent)
	{
		cat("\n\n\t\Šidák-like (1987) step-down Procedure\n\n")
		printRejected(rejected, pValues, NULL)
	}
	return(list(adjPValues=adjPValues, criticalValues=criticalValues, rejected=rejected))
}
#' @export
mutoss.SidakSD <- function() { return(new(Class="MutossMethod",
					label="Šidák-like (1987) step-down",
					errorControl="FWER",
					callFunction="SidakSD",
					output=c("adjPValues", "criticalValues", "rejected"),
					assumptions=c("test independence, "),
					info="<h2>Šidák-like (1987) step-down Procedure </h2>\n\n\
							<h3>Reference:</h3>\
							<ul>\
							<li>Hollander, B.S. and Covenhaver, M.D. \"<i> An Improved Sequentially Rejective Bonferroni Test Procedure.</i>\" Biometrics, 43(2):417-423, 1987. </li>\n\
							</ul>
							<p> The Šidák-like step-down procedure is an improvement over the Holm (1979) step-down procedure. The improvement is analogous to the Šidák's correction over the original Bonferroni procedure. This Šidák-like step-down procedure assumes positive orthant dependent test statistics. </p>\n",
					parameters=list(pValues=list(type="numeric"), alpha=list(type="numeric"))
			)) }





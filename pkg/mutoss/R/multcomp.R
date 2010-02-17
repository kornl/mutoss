# Simultaneous confidence intervals for linear contrasts
# 
# Author: FrankKonietschke
###############################################################################
#' Simultaneous confidence intervals for arbitrary parametric contrasts in unbalanced one-way layouts.
#' The procedure controls the FWER in the strong sense. 
#'

#'With this function, it is possible to compute simultaneous confidence
#'intervals  for arbitrary parametric contrasts in the unbalanced one way layout. Moreover, it computes
#'adjusted p-values. The simultaneous confidence intervals are computed using
#'the multivariate t-distribution. 






#' @param model  a fitted model, for example an object returned by lm, glm, or aov etc. It is
#' assumed that coef and vcov methods are available for model.
#' @param hypotheses a specification of the linear hypotheses to be tested.
#' @param alternative a character string specifying the alternative hypothesis, must be one of 'two.sided'
#' (default), 'greater' or 'less'.
#' @param rhs an optional numeric vector specifying the right hand side of the hypothesis.
#' @param alpha the significance level 
#' @return A list containing:  
#'	\item{adjPValues}{A numeric vector containing the adjusted pValues}
#' 
#'	\item{rejected}{A logical vector indicating which hypotheses are rejected}
#' 
#'  \item{confIntervals}{A matrix containing the estimates and the lower and upper confidence bound}
#' 
#' 	\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function.}
#' 
#' @author MuToss-Coding Team
#' @export
#' @examples
#' data(warpbreaks)
#' # Tukey contrast on the levels of the factor 'Tension'
#' 
#' multcomp.wrapper(aov(breaks ~ tension, data = warpbreaks), hypotheses = "Tukey", alternative="two.sided", factorC="tension",alpha=0.05)
#' 
#' # Williams contrast on 'Tension'
#' multcomp.wrapper(aov(breaks ~ tension, data = warpbreaks), hypotheses= "Williams", alternative="two.sided",alpha=0.05,factorC="tension")
#' 
#' # Userdefined contrast matrix
#' K <-matrix(c(-1,0,1,-1,1,0, -1,0.5,0.5),ncol=3,nrow=3,byrow=TRUE)
#' multcomp.wrapper(aov(breaks ~ tension, data = warpbreaks), hypotheses=K, alternative="two.sided",alpha=0.05,factorC="tension")
#' 
#' # Two-way anova
#' multcomp.wrapper(aov(breaks ~ tension*wool, data = warpbreaks), hypotheses="Tukey", alternative="two.sided",alpha=0.05,factorC="wool")
#' multcomp.wrapper(aov(breaks ~ tension*wool, data = warpbreaks), hypotheses="Tukey", alternative="two.sided",alpha=0.05,factorC="tension")
#' multcomp.wrapper(aov(breaks ~ tension*wool, data = warpbreaks), hypotheses=K, alternative="two.sided",alpha=0.05, factorC="tension")
#' data(iris)
#' multcomp.wrapper(model=lm(Sepal.Length ~ Species, data=iris),hypotheses="Tukey","two.sided",alpha=0.05, factorC="Species")
#' K <-matrix(c(-1,0,1,-1,1,0, -1,0.5,0.5),ncol=3,nrow=3,byrow=TRUE)
#' multcomp.wrapper(model=lm(Sepal.Length ~ Species, data=iris),hypotheses=K,"two.sided",alpha=0.05, factorC="Species")
multcomp.wrapper <- function(model, hypotheses, alternative, rhs=0, alpha, factorC ) {
	
	require("multcomp")	
	
	type<-""
	
	if (any(factorC== c("Tukey", "Dunnett", "Sequen",
			"Williams", "Changepoint", "AVE", "McDermott", "Marcus", "UmbrellaWilliams"))) {
		eval(parse(text=paste("type <- mcp(",factorC,"=hypotheses)")))
	} else {		
		eval(parse(text=paste("type <- mcp(",factorC,"=hypotheses)")))
	}	

	glhtObj <- glht(model, linfct = type, rhs=rhs, alternative=alternative)
	summaryGLHT <- summary(glhtObj)
	pvalues <- summaryGLHT$test$pvalues
	estimates <- summaryGLHT$test$coefficients
	confint <- confint(glhtObj,level=(1-alpha))$confint
	
	
	
	rejected1 <- (pvalues < alpha)
	confi <- cbind(confint)
	print(cbind(confi,pvalues))
	return(list(adjPValues=pvalues,rejected=rejected1,confIntervals= confi,
					errorControl = new(Class='ErrorControl',type="FWER",alpha=alpha)))

	

}


#' @export
mutoss.multcomp<- function() { return(new(Class="MutossMethod",
					label="Multiple Contrast Tests",
					errorControl="FWER",
					callFunction="multcomp.wrapper",
					output=c("adjPValues", "rejected","confIntervals","errorControl"),
					info="<h2>Parametric multiple contrast tests and simultaneous confidence intervals</h2>
							<p>With this function, it is possible to compute simultaneous tests and confidence intervals for general linear hypotheses in parametric models<p> 
							<p></p>
							<h3>Reference:</h3>
							<ul>
							<li>Frank Bretz, Alan Genz and Ludwig A. Hothorn \"<i>On the numerical availability of multiple comparison procedures.</i>\" Biometrical Journal, 43(5), 645-656. , 2001.</li>
													</ul>",
					parameters=list(model=list(type="ANY"),
							hypotheses=list(type="ANY"),
							alpha=list(type="numeric"),
							alternative=list(type="character", label="Alternative", choices=c("two.sided", "less", "greater")),
							factorC=list(type="character", label="Factor for Comparison", fromR="FactorVar")
						
					)
			)) }







#  here I write documentation before putting it in SUD procedures.
# 
# Author: WerftWiebke
###############################################################################

#############################################################################

rm(list=ls())
library(multtest)
data(golub)
data <- golub[1:50,]
dim(golub)
classlabel <- c(rep("positive", 19), rep("negative", 19)) 
multendponecov.model <- function(covariate, addcov) {
	covariate <- as.vector(covariate)
	if (missing(addcov)) {
		return(list(model=list(typ="multendponecov", covariate=covariate)))
	} else {
		addcov <- as.matrix(addcov)## eventuell hier ein cbind
		return(list(model=list(typ="multendponecov", covariate=covariate, addcov=addcov)))
	}
}

model <- multendponecov.model(classlabel)
model <- model$model

lm(data[1,]~model$classlabel "Pr(>|t|)"

multendponecov.marginal <- function(data, model, robust, alternative, psi0) {
	require(MASS)
	if (missing(model$addcov)){
	result <- NULL
	if (robust) {
		result <- apply(data, 1, function(x) {rlm(x$p.value} )
	} else {
		result <- apply(data, 1, function(x) {lm(x~model$covariate)$p.value} )
	}}
	return(list(pValues=result))
}

paired.model <- function(classlabel) {
	classlabel <- as.vector(classlabel)
	return(list(model=list(typ="pairedsamp", classlabel=classlabel)))
}
model <- paired.model(classlabel)
model <- model$model
model$classlabel <- as.factor(model$classlabel)
label <- as.numeric(as.factor(model$classlabel))

x <- data[,label==1]
y <- data[, label==2]
all(cbind(y,x)==data)

res <- paired.marginal(data, model, robust=FALSE, alternative="two.sided", psi0=0)
res2 <- paired.multtest(data, model, alternative="two.sided", robust="FALSE", psi0=0, alpha=0.05, nulldist="boot.cs", B=10000, method="sd.minP", seed=12345)	

result <- apply(data, 1, function(x) { out=x
			anova(lm( out ~ label ))$'Pr(>F)'[1]} )	

library(multtest)
data(golub)
onesamp.model <- function(robust, alternative, psi0) {
	return(list(model=list(typ="onesamp",robust=robust, alternative=alternative, psi0=psi0)))
}
model <- onesamp.model(FALSE, "less", 0)
model <- model$model
data <- golub
typeone="fwer"
k=0
q=0.01
alpha=0.05
nulldist="boot.cs"
method="sd.minP"
seed=0815
B=10
res <- onesamp.multtest(data, model, typeone="fwer", k=0, q=0.01, alpha=0.05, nulldist="boot.cs", B=10, method="sd.minP", seed=0815)



result <- MTP(X=data, W = NULL, Y = NULL, Z = NULL, Z.incl = NULL, Z.test = NULL, 
		na.rm = TRUE, test = "t.onesamp", robust = model$robust, 
		standardize = TRUE, alternative = model$alternative, psi0 = model$psi0, 
		typeone = "fwer", k = k, q = q, fdr.method = "restricted", 
		alpha = 0.05, smooth.null = FALSE, nulldist = nulldist, 
		B = B, ic.quant.trans = FALSE, MVN.method = "mvrnorm", 
		penalty = 1e-06, method = method, get.cr = FALSE, get.cutoff = FALSE, 
		get.adjp = TRUE, keep.nulldist = FALSE, keep.rawdist = FALSE, 
		seed = seed, cluster = 1, type = NULL, dispatch = NULL, marg.null = NULL, 
		marg.par = NULL, keep.margpar = TRUE, ncp = NULL, perm.mat = NULL, 
		keep.index = FALSE, keep.label = FALSE)





#' Benjamini-Liu's step-down procedure is applied to pValues derived from test statitics. 
#' The procedure controls the FDR if the test statistics are stochastically independent.
#' The Benjamini-Liu (BL) step-down procedure neither dominates nor is dominated by the Benjamini-Hochberg (BH) step-up procedure.
#' However, in Benjamini and Liu (1999) a large simulation study concerning the power of the two procedures reveals that the BL step-down procedure is more suitable when the number of hypotheses is small. 
#' Moreover, if most null hypotheses are far from being true then the BL step-down procedure is more powerful than the BH step-up method.
#' 
#' The BL step-down method calculates critical values according to Benjamin and Liu (1999), 
#' i.e. c_i = 1 - (1 - min(1, (m*alpha)/(m-i+1)))^(1/(m-i+1)) for i = 1,...,m 
#' where m is the number of hypotheses tested (m=length(pValues)). 
#' Then, let k be the smallest i for which P_(i) > c_i and reject H_(1),...,H_(k−1). 
#' Note that if P_(i) <= c_i for i = 1,...,m then all the m hypotheses are rejected.
#' 
#' @title Benjamini-Liu (1999) step-down procedure
#' @param pValues unadjusted pValues to be used. 
#' @param alpha the level at which the FDR shall be controlled.
#' @param silent if true any output on the console will be suppressed.
#' @return A list with adjPValues, criticalValues and rejected is returned.
#' 
#'	\item{adjPValues}{a numeric vector containing the adjusted pValues}
#' 
#'	\item{rejected}{a logical vector indicating which hypotheses are rejected}
#' 
#'	\item{criticalValues}{a numeric vector containing critical values used in the step-up-down test} 
#' @author WerftWiebke
#' @references Bejamini, Y. and Liu, W. (1999). A step-down multiple hypotheses testing procedure that controls the false discovery rate under independence . 
#' 				Journal of Statistical Planning and Inference Vol. 82(1-2): 163-170. 
#' @export
#' @examples 
#' alpha <- 0.05
#' p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9, max=1))
#' result <- BL(p, alpha)
#' result <- BL(p, alpha, silent=TRUE)


####################################################################


#' Benjamini-Hochbergs step-up procedure is applied to pValues. 
#' The procedure controls the FDR if the test statistics are stochastically independent. It also controls the false discovery
#' rate when the test statistics have positive regression dependency on each of the test statistics corresponding to the true null hypotheses (Benjamini and Yekutieli, 2001).
#' 
#' The Benjamini-Hochberg (BH) step-up procedure considers ordered pValues P_(i). It defines k as the largest i for which P_(i) <= i*alpha/m and then 
#' rejects all H_(i) for i=1,...,k. In Benjamini and Hochberg (1995) it is shown that for 0<=m_0<=m independent pValues corresponding to true null hypotheses 
#' and for any joint distribution of thr m_1=m-m_0 p-values corresponding to the false null hypotheses the FDR is controlled at level (m_0/m)*alpha.  Under the assumption of the PRDS property, i.e. 
#' positive regression dependency on each of the test statistics corresponding to the true null hypotheses, FDR control at level (m_0/m)*alpha is shown (for details see Benjamini and Yekutieli (2001)). 
#' In Benjamini et al. (2006) the BH procedure is improved by adaptive procedures which use an estimate of m0 and apply the BH method to alpha'=alpha*m/m_0, to achieve precisely the desired level alpha (see adaptiveBH, twostageBKY, multistageBKY, twostageSTS).        
#' @title Benjamini-Hochberg (1995) linear step-up procedure
#' @param pValues the used unadjusted pValues.
#' @param alpha the level at which the FDR shall be controlled.
#' @param silent if true any output on the console will be suppressed. 
#' @return A list with adjPValues, criticalValues and rejected is returned.
#' 
#'	\item{adjPValues}{a numeric vector containing the adjusted pValues}
#' 
#' 	\item{criticalValues}{a numeric vector containing critical values used in the step-up test} 
#'	
#' 	\item{rejected}{a logical vector indicating which hypotheses are rejected}
#' 	
#' @author WerftWiebke
#' @references 	Bejamini, Y. and Hochberg, Y. (1995). Controlling the false discovery rate: A practical and powerful approach to mulitple testing.
#'					 Journal of the Royal Statistical Society, Series B, 57:289-300.\eqn{n}
#' 
#' 				Bejamini, Y. and Yekutieli, D. (2001). The control of the false discovery rate in multiple testing under dependency.
#' 				 Annals of Statistics, 29(4):1165-1188.\eqn{n}
#' 
#' 				Benjamini, Y., Krieger, A. and Yekutieli, D. (2006). Adaptive linear step-up procedures that control the false
#' 							discovery rate. Biometrika, 93(3):491–507.
#' @export
#' @examples 
#' alpha <- 0.05
#' p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9, max=1))
#' result <- BH(p, alpha)
#' result <- BH(p, alpha, silent=TRUE)

#############################################################################


#' The Benjamini-Yekutieli step-up procedure is applied to pValues. 
#' The procedure ensures FDR control for any dependency structure.
#' 
#' The critical values of the Benjamini-Yekutieli (BY) procedure are calculated by 
#' replacing the alpha of the Benjamini-Hochberg procedure by alpha/sum(1/1:m)), i.e.
#' c(i)=i*alpha/m*(sum(1/1:m)) for i=1,...,m. Benjamini and Yekutieli (2001) showed that this step-up procedure controls
#' the FDR at level alpha*m/m0 for any test statistics dependency structure.
#' 
#' @title Benjamini-Yekutieli (2001) step-up procedure
#' @param pValues the used unadjusted pValues.
#' @param alpha the level at which the FDR shall be controlled.
#' @param silent if true any output on the console will be suppressed. 
#' @return A list with adjPValues, criticalValues and rejected is returned.
#' 
#' \item{adjPValues}{a numeric vector containing the adjusted pValues}
#' 
#' \item{criticalValues}{a numeric vector containing critical values used in the step-up-down test} 
#' 
#' \item{rejected}{a logical vector indicating which hypotheses are rejected}
#' 
#' @author WerftWiebke
#' @references 	Bejamini, Y. and Yekutieli, D. (2001). The control of the false discovery rate in multiple testing under dependency.
#' 				 Annals of Statistics, 29(4):1165-1188. 
#' @export
#' @examples 
#' alpha <- 0.05
#' p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9, max=1))
#' result <- BY(p, alpha)
#' result <- BY(p, alpha, silent=TRUE)

############################################################################


#' The Hochberg step-up procedure is based on individual p-values. It controls the FWER in the strong 
#' sense under joint null distributions of the test statistics that satisfy Simes' inequality.
#' 
#' The Hochberg procedure is more powerful than Holm's (1979) procedure, but the test statistics need to be
#' independent or have a distribution with multivariate total positivity of order two or a scale mixture
#' thereof for its validity (Sarkar, 1998). 
#' Both procedures use the same set of critical values c(i)=alpha/(m-i+1). Whereas Holm's procedure is a step-down 
#' version of the Bonferroni test, and Hochberg's is a step-up version of the Bonferroni test.
#' Note that Holm’s method is based on the Bonferroni inequality and is valid regardless of the joint
#' distribution of the test statistics.
#' @title Hochberg (1988) step-up procedure
#' @param pValues the used pValues.
#' @param alpha the level at which the FDR shall be controlled.
#' @param silent if true any output on the console will be suppressed. 
#' @return A list with adjPValues, criticalValues and rejected is returned.
#' 
#'	\item{adjPValues}{a numeric vector containing the adjusted pValues}
#' 
#'	\item{rejected}{a logical vector indicating which hypotheses are rejected}
#' 
#'	\item{criticalValues}{a numeric vector containing critical values used in the step-up-down test} 
#' @author WerftWiebke
#' @references 	Hochberg, Y. (1988). A sharper Bonferroni procedure for multiple tests of significance. 
#' 		Biometrika, 75:800-802.\eqn{n}
#' 		Huang, Y. and Hsu, J. (2007). Hochberg's step-up method: cutting corners off Holm's step-down method. Biometrika, 94(4):965-975.
#' @export
#' @examples 
#' alpha <- 0.05
#' p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9,max=1))
#' result <- Hochberg(p, alpha)
#' result <- Hochberg(p, alpha, silent=TRUE)


########################################################################



#' Holm's step-down-procedure is applied to pValues. It controls
#' the FWER in the strong sense under arbitrary dependency.
#' 
#' Holm's procedure uses the same critical values as the Hochberg's procedure, namely  c(i)=alpha/(m-i+1), 
#' but is a step-down version while Hochberg's method is a step-up version of the Bonferroni test.
#' Holm’s method is based on the Bonferroni inequality and is valid regardless of the joint
#' distribution of the test statistics. Whereas Hochberg's method relies on the assumption that 
#' Simes' inequality holds for the joint null distribution of the test statistics. If this assumption is met Hochberg's
#' step-up procedure is more powerful than Holm's step-down procedure.  
#' @title Holm's (1979) step-down-procedure
#' @param pValues pValues to be used. They can have arbitrary dependency structure. 
#' @param alpha the level at which the FWER shall be controlled
#' @param silent if true any output on the console will be suppressed.  
#' @return List with adjusted pValues, rejected and critical values. 
#' 
#'	\item{adjPValues}{a numeric vector containing the adjusted pValues}
#' 
#'	\item{rejected}{a logical vector indicating which hypotheses are rejected}
#' 
#'	\item{criticalValues}{a numeric vector containing critical values used in the step-up-down test} 
#' @author MarselScheer
#' @references S. Holm (1979). A simple sequentially rejective multiple 
#' 		test procedure. Scand. J. Statist. Vol. 6, 65-70. \eqn{n}
#' 		
#' Huang, Y. and Hsu, J. (2007). Hochberg's step-up method: cutting corners off Holm's step-down method. Biometrika, 94(4):965-975.
#' @export
#' @examples 
#' r <- c(runif(50), runif(50, 0, 0.01))
#' result 	<- holm(r, 0.05)
#' result 	<- holm(r, 0.05, silent = TRUE)

#################################################################

#' The adaptive Benjamini-Hochberg step-up procedure is applied to pValues.
#' It controls the FDR at level alpha for independent or positive regression dependent test statistics.
#' 
#' In the adaptive Benjamini-Hochberg step-up procedure the number of true null hypotheses is estimated first as in Hochberg and
#' Benjamini (1990), and this estimate is used in the procedure of Benjamini and
#' Hochberg (1995) with alpha'=alpha*m/m0. 
#' @title Benjamini-Hochberg (2000) adaptive linear step-up Procedure
#' @param pValues the used pValues.
#' @param alpha the level at which the FDR shall be controlled.
#' @param silent if true any output on the console will be suppressed. 
#' @return A list with adjPValues, criticalValues and rejected is returned.
#' 
#'	\item{adjPValues}{a numeric vector containing the adjusted pValues}
#' 
#' \item{criticalValues}{a numeric vector containing critical values used in the step-up-down test} 
#' 
#'	\item{rejected}{a logical vector indicating which hypotheses are rejected}
#' 
#' \item{pi0}{an estimate of the proportion of true null hypotheses among all hypotheses (pi0=m0/m). }
#' 
#' @author WerftWiebke
#' @references 	Benjamini, Y. and Hochberg, Y. (2000). On the Adaptive Control of the False Discovery Rate
#' 							in Multiple Testing With Independent Statistics.
#' 							Journal of Educational and Behavioral Statistics, 25(1): 60-83.\eqn{n}
#' 
#' 							Hochberg, Y. and Benjamini, Y. (1990). More powerful procedures for multiple significance testing. 
#' 							Statistics in Medicine 9, 811-818.\eqn{n}
#' 
#' 							Bejamini, Y. and Hochberg, Y. (1995). Controlling the false discovery rate: A practical and powerful approach to mulitple testing.
#'							 Journal of the Royal Statistical Society, Series B, 57:289-300.
#' @export
#' @examples 
#' alpha <- 0.05
#' p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9, max=1))
#' result <- adaptiveBH(p, alpha)
#' result <- adaptiveBH(p, alpha, silent=TRUE)




#' The Benjamini-Krieger-Yekutieli two-stage procedure is applied to pValues. The procedure controls
#' the FDR at level alpha for independent or positive regression dependent test statistics.
#' 
#' In the Benjamini-Krieger-Yekutieli two-stage procedure the linear step-up procedure is used in
#' stage one to estimate m0, providing a new level alpha'=alpha*m/m0 which is used in the linear step-up
#' procedure in the second stage. Benjamini et al. (2006) prove that a general form of the two-stage procedure
#' controls the false discovery rate at the desired level alpha.
#' @title Benjamini-Krieger-Yekutieli (2006) two-stage step-up Procedure
#' @param pValues the used pValues.
#' @param alpha the level at which the FDR shall be controlled. 
#' @param silent if true any output on the console will be suppressed. 
#' @return A list with criticalValues, rejected and pi0 is returned.
#' 
#'	\item{rejected}{a logical vector indicating which hypotheses are rejected}
#' 
#'	\item{criticalValues}{a numeric vector containing critical values used in the step-up-down test} 
#' 
#' \item{pi0}{an estimate of the proportion of true null hypotheses among all hypotheses (pi0=m0/m). }
#' @author WerftWiebke
#' @references 	Benjamini, Y., Krieger, A. and Yekutieli, D. (2006). Adaptive linear step-up procedures that control the false
#' 							discovery rate. 
#' 		Biometrika, 93(3):491–507.
#' @export
#' @examples 
#' alpha <- 0.05
#' p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9,max=1))
#' result <- twostageBKY(p, alpha)
#' result <- twostageBKY(p, alpha, silent=TRUE)


########################################################################


#'
#' The Šidák-like step-down procedure is an improvement over the Holm (1979) 
#' step-down procedure. The improvement is analogous to the Šidák's correction 
#' over the original Bonferroni procedure. This Šidák-like step-down procedure 
#' assumes positive orthant dependent test statistics.
#' @title  Šidák-like (1987) step-down procedure
#' @param pValues the used pValues.
#' @param alpha the level at which the FDR shall be controlled.
#' @param silent if true any output on the console will be suppressed. 
#' @return A list with adjPValues, criticalValues and rejected is returned.
#' 
#'	\item{adjPValues}{a numeric vector containing the adjusted pValues}
#' 
#'	\item{rejected}{a logical vector indicating which hypotheses are rejected}
#' 
#'	\item{criticalValues}{a numeric vector containing critical values used in the step-up-down test} 
#' @author WerftWiebke
#' @references 	Hollander, B.S. and Covenhaver, M.D. (1987). An Improved Sequentially Rejective Bonferroni Test Procedure.
#' 		Biometrics, 43(2):417-423, 1987.
#' @export
#' @examples 
#' alpha <- 0.05
#' p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9,max=1))
#' result <- SidakSD(p, alpha)
#' result <- SidakSD(p, alpha, silent=TRUE)


### Masterplan von Marsel ###

#' Rom's step-up-procedure is applied to pValues. The procedure 
#' controls the FWER in the strong sense if the pValues are
#' stochastically independent.
#'  
#' This function calculates the critical values by the formula given
#' in Finner, H. and Roters, M. (2002). After that a step-up test
#' is performed to reject hypotheses associated with pValues.
#'  
#' Since the formula for the critical values is recursive, it is at the
#' moment not possible to calculate adjusted pValues. Maybe in future there
#' will be at least a numerically solution for this issue.
#' @title Rom's (1990) step-up-procedure.
#' @param pValues pValues to be used. They are assumed to be stochastically independent. 
#' @param alpha the level at which the FWER shall be controlled.
#' @param silent if true any output on the console will be suppressed.  
#' @return A list with adjPValues, criticalValues and rejected is returned.
#'	\item{adjPValues}{a numeric vector containing the adjusted pValues}
#' 
#'	\item{rejected}{a logical vector indicating which hypotheses are rejected}
#' 
#'	\item{criticalValues}{a numeric vector containing critical values used in the step-up-down test} 

#' @author MarselScheer
#' @references 	Rom, D. M. (1990). A sequentially rejective test 
#' 		procedure based on a modified Bonferroni inequality. 
#' 		Biometrika 77, 663–665. \eqn{n}
#' 
#' 		Finner, H. and Roters, M. (2002). Multiple hypotheses testing and
#' 		expected type I errors. Ann. Statist. 30, 220–238.
#' @export
#' @examples 
#' r <- c(runif(50), runif(50, 0, 0.01))
#' result <- rom(r, 0.05)
#' result <- rom(r, 0.05, silent = TRUE)
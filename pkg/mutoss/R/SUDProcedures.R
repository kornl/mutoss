# Here all implemented concrete SUD-Procedures can be found.  
# 
# Author: MarselScheer and WerftWiebke
###############################################################################


#++++++++++++++++++++++++++++   OutputFkt    +++++++++++++++++++++
#' Generates standard output for pValues, rejected and
#' adjustedPValues.
#' 
#' It generates an output on the console with the number of
#' hypotheses (number of pValues) and the number of rejected
#' hypotheses (number of rejected pValues). Further a data.frame
#' is constructed, one column containing the rejected pValues, 
#' one the index number of the rejected pValues and if given 
#' one column with the corresponding adjusted pValues.
#' @title Internal MuTossProjekt-Function
#' @param rejected logical Vector indicating which pValue is rejected. 
#' @param pValues the used pValues.
#' @param adjPValues the adjusted pValues. 
#' @author MarselScheer
#' @export
printRejected = function(rejected, pValues = NULL, adjPValues = NULL) 
{
	cat("Number of hyp.:\t", length(rejected), "\n")
	cat("Number of rej.:\t", sum(rejected), "\n")
	idx <- which(rejected)
	
	if (length(idx) != 0)
	{
		output <- data.frame(rejected = idx)
		if (!is.null(pValues))
		{
			output <- data.frame(output, pValues[idx])
			names(output)[length(names(output))] <- "pValues"
		}
		
		if (!is.null(adjPValues))
		{
			output <- data.frame(output, adjPValues[idx])
			names(output)[length(names(output))] <- "adjPValues"
		}
		
		
		if (!is.null(pValues))
			# sorting by pValues
			output 	<- output[order(output$pValue), ]
		else
		{
			if (!is.null(adjPValues))
				# no pValues availible, sorting by adjPValues
				output 	<- output[order(output$adjPValues), ]			
		}
		
		rownames(output) <- 1:length(idx)
		
		print(output)
	}
}
#----------------------------   OutputFkt    ---------------------

#Rom_simpleImplementation <- function(pValues, alpha) 
#{
#	# ROM, D. M. (1990). A sequentially rejective test procedure based 
#	# on a modified Bonferroni inequality. Biometrika 77, 663-665.
#	
#	# Formula for the critical values is taken from
#	# FINNER, H. and ROTERS, M. (2002). Multiple hypotheses testing and 
#	# expected type I errors. Ann. Statist. 30, 220-238.
#	# Notice: 	The smallest critical value in this paper is alpha_1!!	
#	#			Thus the critical values are calculated in this manner, and
#	#			at the end the order is reversed.
#	
#	# ++++++ Calculating critical values
#	
#	# TODO: Perhaps there are computational problems if too many hypotheses are tested.
#	len 		<- length(pValues)
#	criticalValues 	<- rep(0, times=len)
#	criticalValues[1]	<- alpha
#	
#	# TODO: !! firstSum_k[14:len] is constant for example if alpha=5%, len=200
#	# firstSum_k := SUMME(alpha^i, i=1..(k-1))	
#	firstSum_k <- cumsum(c(0, sapply(1:(len-1), function(i) alpha^i)))
#	
#	criticalValues[2] <- 1/2 * (firstSum_k[2] - 0)
#	secondSummand <- function(i) choose(k,i) * criticalValues[i+1]^(k-i)
#	for (k in 3:len)
#	{		
#		# TODO: secondSum can be calculated faster!
#		secondSum 	<- sum(sapply(1:(k-2), secondSummand))
#		criticalValues[k] 	<- 1 / k * (firstSum_k[k] - secondSum)  
#		#cat("1 ", secondSum, "\n")
#	}	 
#	criticalValues <- criticalValues[len:1]
#
#	# ------ Calculating critical Values
#
#	SU(pValues, criticalValues)
#
#}


# TODO: MS !! Discussion about big n !!
#' Rom's step-up-procedure is applied to pValues. The procedure 
#' controls the FWER in the strong sense if the pValues are
#' stochastically independent.
#'  
#' This function calculates the critical values by the formula given
#' in Finner, H. and Roters, M. (2002) based on the joint distribution
#' of order statistics. After that a step-up test
#' is performed to reject hypotheses associated with pValues.
#'  
#' Since the formula for the critical values is recursive,
#' the calculation of adjusted pValues is far from obvious and is
#' not implemented here. 
#' 
#' @title Rom's (1990) step-up-procedure.
#' @param pValues pValues to be used. They are assumed to be stochastically independent. 
#' @param alpha the level at which the FWER shall be controlled.
#' @param silent if true any output on the console will be suppressed.  
#' @return A list containing:
#'	\item{adjPValues}{A numeric vector containing the adjusted pValues}
#' 
#'	\item{rejected}{A logical vector indicating which hypotheses are rejected.}
#' 
#'	\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test.}
#' 
#' 	\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}
#' @author Marsel Scheer
#' @references 	Rom, D. M. (1990). A sequentially rejective test 
#' 		procedure based on a modified Bonferroni inequality. 
#' 		Biometrika 77, 663-665.
#' 
#' 		Finner, H. and Roters, M. (2002). Multiple hypotheses testing and
#' 		expected type I errors. Ann. Statist. 30, 220-238.
#' @export
#' @examples 
#' r <- c(runif(50), runif(50, 0, 0.01))
#' result <- rom(r, 0.05)
#' result <- rom(r, 0.05, silent = TRUE)
rom <- function(pValues, alpha, silent = FALSE) 
{
	# 
	# Remark: The critical values calculated by this procedure were
	#			compared with the critical values calculated by Rom
	#			himself in his paper and they are the same.
	
	
	# Formula for the critical values is taken from
	# FINNER, H. and ROTERS, M. (2002). Multiple hypotheses testing and 
	# expected type I errors. Ann. Statist. 30, 220-238.
	# Notice: 	The smallest critical value in this paper is alpha_1!!	
	#			Thus the critical values are calculated in this manner, and
	#			at the end the order is reversed.
	
	# ++++++ Calculating critical Values
	
	# TODO: MS perhaps there are computational problems if too many hypotheses are tested.
	len 		<- length(pValues)
	criticalValues 	<- rep(0, times=len)
	criticalValues[1]	<- alpha
	
	# TODO: MS !! firstSum_k[14:len] is constant for example if alpha=5%, len=200
	# firstSum_k 	:= SUMME(alpha^i, i=1..(k-1))
	firstSum_k 	<- cumsum(c(0, sapply(1:(len-1), function(i) alpha^i))) 
	
	criticalValues[2] <- 1/2 * (firstSum_k[2] - 0)
	
	# SUMME(binomial(k,i) * alpha_{i+1}^{k-i}, i=1..(k-2))
	# = SUMME(aki, i=1..(k-2))
	# = SUMME(binomial(k-1, i) * k / (k-i) * a(k-1)i * alpha_{i+1}, i=1..(k-2))		
	# for k = 3 and i = 1
	# aki and binKoef actually has 2 dimensions, the k-dimension and the i-dimension. 
	# But in this code we will only work with the i-dimension. In every step of 
	# the for-loop aki[i] will be updated.
	aki 	<- rep(0, times = (len-2))	
	binKoef <- rep(1, times = (len-2))
	
	binKoef[1] 	<- choose(3,1)
	aki[1] 		<- criticalValues[2]^(3-1)
	
	for (k in 3:len)
	{						
		secondSummand 		<- sum(binKoef[1:(k-2)] * aki[1:(k-2)])
		criticalValues[k] 			<- 1/k * (firstSum_k[k] - secondSummand)
		
		# updating the vectors for the next step
		binKoef[k-1]		<- (k+1) * k / 2 #choose(k+1, k-1)
		binKoef[1:(k-2)]	<- binKoef[1:(k-2)] * (k+1) / (k:3)	
		
		aki[k-1]			<- criticalValues[k]^2
		aki[1:(k-2)]		<- aki[1:(k-2)] * criticalValues[2:(k-1)]
	}	 
	criticalValues <- criticalValues[len:1]
	# ------ Calculating critical Values
	
	rejected <- SU(pValues, criticalValues)
	
	if (! silent)
	{
		cat("\n\n\t\tRom's (1990) step-up procedure\n\n")
		printRejected(rejected, pValues, NULL)
	}
	
	# TODO: MS calculating adjustedPValues for ROM numerically
	return(list(rejected = rejected, criticalValues = criticalValues,
					errorControl = new(Class='ErrorControl',type="FWER",alpha=alpha))
	)
}


#' @export 
#' @nord
mutoss.rom <- function() { return(new(Class="MutossMethod",
					label="Rom's (1990) step-up",
					errorControl="FWER",
					callFunction="rom",
					output=c("rejected", "criticalValues","errorControl"),
					info="<h2>Rom's step-up procedure</h2>\n\n\
							<p>Rom's step-up-procedure is applied to pValues. The procedure 
                               controls the FWER in the strong sense if the pValues are
 							   stochastically independent.</p>
  
                           <p> This function calculates the critical values by the formula given
                               in Finner, H. and Roters, M. (2002) based on the joint distribution
								of order statistics. After that a step-up test
                               is performed to reject hypotheses associated with pValues.</p>
  
                           <p> Since the formula for the critical values is recursive,
                               the calculation of adjusted pValues is far from obvious and is
                               not implemented here.</p>
							<h3>Reference:</h3>\
							<ul>\
							<li>Rom, D. M. \"<i> A sequentially rejective test procedure based on a modified Bonferroni inequality. </i>\" Biometrika 77, 663-665. </li>\n\
							</ul>",
					parameters=list(pValues=list(type="numeric"), alpha=list(type="numeric"))
			)) }

#-------------------- Holm's Step-down--------------------#

#' Holm's step-down-procedure is applied to pValues. It controls
#' the FWER in the strong sense under arbitrary dependency.
#' 
#' Holm's procedure uses the same critical values as Hochberg's procedure, namely  c(i)=alpha/(m-i+1), 
#' but is a step-down version while Hochberg's method is a step-up version of the Bonferroni test.
#' Holm's method is based on the Bonferroni inequality and is valid regardless of the joint
#' distribution of the test statistics, whereas Hochberg's method relies on the assumption that 
#' Simes' inequality holds for the joint null distribution of the test statistics. If this assumption is met, Hochberg's
#' step-up procedure is more powerful than Holm's step-down procedure.  
#' @title Holm's (1979) step-down-procedure
#' @param pValues pValues to be used. They can have arbitrary dependency structure. 
#' @param alpha The level at which the FWER shall be controlled
#' @param silent If true any output on the console will be suppressed.  
#' @return A list containing: 
#' 
#'	\item{adjPValues}{A numeric vector containing the adjusted pValues}
#' 
#'	\item{rejected}{A logical vector indicating which hypotheses are rejected}
#' 
#'	\item{criticalValues}{A numeric vector containing critical values used in the step-down test}
#' 
#' 	\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}
#' @author MarselScheer
#' @references S. Holm (1979). A simple sequentially rejective multiple 
#' 		test procedure. Scand. J. Statist. Vol. 6, 65-70. \eqn{n}
#' 
#' 		Huang, Y. and Hsu, J. (2007). Hochberg's step-up method: cutting corners off Holm's step-down method. Biometrika, 94(4):965-975.
#' @export
#' @examples 
#' r <- c(runif(50), runif(50, 0, 0.01))
#' result 	<- holm(r, 0.05)
#' result 	<- holm(r, 0.05, silent = TRUE)
holm <- function(pValues, alpha, silent = FALSE) 
{
	m <- length(pValues)
	criticalValues <- sapply(1:m, function(i) alpha/(m-i+1))
	adjPValues <- p.adjust(pValues, "holm")
	rejected <- (adjPValues <= alpha)
	if (! silent)
	{
		cat("\n\n\t\tHolm's (1979) step-down Procedure\n\n")
		printRejected(rejected, pValues, adjPValues)
	}
	
	return(list(adjPValues = adjPValues,
			rejected = rejected, criticalValues=criticalValues,
			errorControl = new(Class='ErrorControl',type="FWER",alpha=alpha))
	)
}

#' @export 
#' @nord
mutoss.holm <- function() { return(new(Class="MutossMethod",
					label="Holm's (1979) step-down",
					errorControl="FWER",
					callFunction="holm",
					output=c("adjPValues", "rejected", "criticalValues","errorControl"),
					info="<h2>Holm's step-down-procedure</h2>\n\n\
							<p> Holm's step-down-procedure is applied to pValues. It controls
								the FWER in the strong sense under arbitrary dependency.</p>\n\
							<p>	Holm's procedure uses the same critical values as the Hochberg's procedure, namely  <i>c(i)=&alpha;/(m-i+1)</i>, 
								but is a step-down version while Hochberg's method is a step-up version of the Bonferroni test.
								Holm's method is based on the Bonferroni inequality and is valid regardless of the joint
								distribution of the test statistics, whereas Hochberg's method relies on the assumption that 
								Simes' inequality holds for the joint null distribution of the test statistics. If this assumption is met, Hochberg's
								step-up procedure is more powerful than Holm's step-down procedure.</p>\n\
							<h3>Reference:</h3>\
							<ul>\
							<li>Holm, S. (1979). \"<i> A simple sequentially rejective multiple test procedure. </i>\" Scand. J. Statist. Vol. 6, 65-70. </li>\n\
							<li>Huang, Y. and Hsu, J. (2007). \"<i> Hochberg's step-up method: cutting corners off Holm's step-down method. </i>\" Biometrika, 94(4):965-975.</li>
							</ul>",
					parameters=list(pValues=list(type="numeric"), alpha=list(type="numeric"))
			)) }


# TODO: MS Probably jointCDF.unif should probably be moved to some math.R or so.
# TODO: MS !! jointCDF.unif: There are numerical issues because the of accuracy of doublePrecison
# TODO: MS !! How to communicate numerical issues to the user. 
#' Calculates the joint cumulative distribution function of order statistics 
#' of n iid. U(0,1)-distributed random variables at argument vec. 
#' Because of numerical issues n should not be greater than 100. 
#' 
#' Following Shorack, Wellner (1986) or Finner, Roters (2002) by applying
#' Bolshev's recursion the joint distribution is calculated. 
#' @title Joint cumulative distribution function of order statistics of n iid. U(0,1)-distributed random variables
#' @param vec a numeric vector. The length of the vector also
#' 		determines the number of random variables considered. 
#' @return The return value is the following probability 
#' 		P(U_(1:n) <= vec[1], ..., U_(n:n) <= vec[n]), where
#' 		U_1, ..., U_n are assumed to be iid. uniformly 
#' 		distributed on [0,1]. The i-th ordered value is denoted by
#' 		U_(i:n) and n equals length(vec)
#' @references 
#' 		Shorack, G. R. and Wellner, J. A. (1986). 
#' 			Empirical Processes with Applications to Statistics.
#' 			Wiley, New York.
#' 
#' 		Finner, H. and Roters, M. (2002). Multiple hypotheses testing and
#' 		expected type I errors. Ann. Statist. 30, 220-238. 
#' @author MarselScheer
jointCDF.orderedUnif = function(vec) 
{
	# vec is not ordered. Thus the probability must be 0
	if (!all(order(vec) == 1:length(vec)))
	{		
		print("ORDER!")
		print(vec)
		print(order(vec))
		vecName <- deparse(substitute(vec))
		warning(paste("jointCDF.unif(): The variable", vecName, "is not ordered. Thus the probability is 0!"))
		return(0)
	}
	
	if (min(vec) <= 0) return(0)
	vec[ vec > 1 ] <- 1
	
	if (100 < length(vec))
		warning("Length of the argument is longer than 100. Calculated value may not be useable!")
	
	# By Bolshev's recursion
	# 	P(U_{1:n} <= vec[1], ..., U_{n:n} <= vec[n])
	# 	= Fn(vec[1], ..., vec[n])
	# 	= 1 - sum( binom(n, j) * Fj(vec[1], ..., vec[j]) * (1-vec[j+1])^(n-j), j=0..n-1)
	# with F0 = 1.
	# The variable Fj[k+1] used in this function will correspond to
	# Fk(vec[1], ..., vec[k]) for all k = 0 .. n.
	# So Fj[1] is F0 = 1, Fj[2] is F1(vec[1]) and so on. 
	Fj <- rep(0, times = length(vec) + 1)	
	
	Fj[1] <- 1 # F0
	# consider k; 
	# 	Fj[k+1]; 
	#	Fk(vec[1], ..., vec[k]) 
	#	= 1 - sum( binom(k, s) * Fs(vec[1], ..., vec[s]) * (1-vec[s+1])^(n-s) , s=0..k-1 )
	#	= 1 - sum( choose(k,s) * Fj[s+1] * (1 - vec[s+1])^(k-s), s=0..k-1 )
	
	summand <- function(s) choose(k,s) * Fj[s+1] * (1 - vec[s+1])^(k-s)
	
	
	for(k in 1:length(vec))
		Fj[k+1] <- 1 - sum( sapply(0:(k-1), summand))
	
	return(Fj[length(vec)+1])
}


#' Calculates the beta to adjust the asymptotically optimal rejection curve
#' used by the function aorc() for a finite sample size. Then  
#' aorc(..., betaAdjustment = beta) controls the FDR also in the
#' finite sample situation.
#' 
#' The asymptotically optimal rejection curve, denoted by f(t), does not 
#' provide finite control of the FDR. calculateBetaAdjustment() calculates
#' a factor, denoted by beta, such that (1 + beta/n) * f(t) provides
#' finite control of the FDR.
#' 
#' The beta is calculated with the bisection approach. Assume there are beta1
#' and beta2 such that the choosing beta1 controls the FDR and beta2 not, then the 
#' optimal beta lies in [beta2, beta1]. If the choice (beta2 + beta1)/2 controls
#' the FDR, the optimal FDR lies in [(beta2 + beta1)/2, beta1]
#' and so on. 
#' @title Calculating the beta adjustment factor for the asymptotically optimal rejection curve.    
#' @param n Number of tests for which the adjusted beta should be calculated. 
#' @param startIDX_SUD Starting index of the step-up-down procedure 
#' @param alpha The level at which the FDR shall be controlled.
#' @param silent If true any output on the console will be suppressed.
#' @param initialBeta  
#' @param maxBinarySteps 
#' @param tolerance 
#' @return The adjustment factor that is needed to ensure control of
#' 		the FDR with the adjusted asymptotically optimal rejection curve
#' 		at the specified level and sample size.
#' @author MarselScheer
#' @export
calculateBetaAdjustment = function(n, startIDX_SUD, alpha, silent = FALSE, initialBeta = 1, maxBinarySteps = 50, tolerance = 0.0001) 
{
	#+++++++++++++++++++++++++++   Subfunctions   +++++++++++++++++++++++++
	#probability mass function
	pmf <- function(criticalValues, startIDX_SUD, n, n0, j) 
	{# Calculates P_{n,n0}(V = j) for a set of critical Values.
		# Formulas are from Finner, Gontscharuk, Dickhaus: FDR controlling step-up-down tests
		# 	related to the asmptotically optimal rejection curve. (to appear)
		if (n0 < j) return(0)
		
		n1 <- n - n0
		
		if (startIDX_SUD <= n1)
		{
			if (j == 0)
				return(	choose(n0,j)
								* 1 
								* (1-criticalValues[n1 + j + 1])^(n0-j)
				)
			return(	choose(n0,j)
							* jointCDF.orderedUnif(criticalValues[(n1 + 1):(n1 + j)]) 
							* (1-criticalValues[n1 + j + 1])^(n0-j)
			)
		}
		
		if ((n1 < startIDX_SUD) && (j < startIDX_SUD - n1 - 1))
		{			
			if (n1 + j == 0) # <=> n1 == 0 and j == 0
				return(choose(n0,j)
								* jointCDF.orderedUnif(c(rep(1 - criticalValues[startIDX_SUD], times = (n - startIDX_SUD + 1)), 1 - criticalValues[(startIDX_SUD - 1):(n1 + j + 1)]))
								* 1			
				)
			return(choose(n0,j)
							* jointCDF.orderedUnif(c(rep(1 - criticalValues[startIDX_SUD], times = (n - startIDX_SUD + 1)), 1 - criticalValues[(startIDX_SUD - 1):(n1 + j + 1)]))
							* criticalValues[n1 + j]^j
			)
		}
		
		if ((n1 < startIDX_SUD) && (j == startIDX_SUD - n1 - 1))
		{
			if (n1 + j == 0) # <=> n1 == 0 and j == 0
				return(choose(n0,j)
								* jointCDF.orderedUnif(rep(1 - criticalValues[startIDX_SUD], times = (n - startIDX_SUD + 1)))
								* 1
				)			
			return(choose(n0,j)
							* jointCDF.orderedUnif(rep(1 - criticalValues[startIDX_SUD], times = (n - startIDX_SUD + 1)))
							* criticalValues[n1 + j]^j
			)
		}
		
		
		if ((n1 < startIDX_SUD) && (j == startIDX_SUD - n1))
			return(choose(n0,j)
							* jointCDF.orderedUnif(rep(criticalValues[startIDX_SUD], times=j))
							* (1 - criticalValues[n1 + j + 1])^(n0 - j)
			)
		
		if ((n1 < startIDX_SUD) && (startIDX_SUD - n1 < j))
			return(choose(n0,j)
							* jointCDF.orderedUnif(c(rep(criticalValues[startIDX_SUD], times=(startIDX_SUD - n1)), criticalValues[(startIDX_SUD + 1):(n1 + j)]))
							* (1 - criticalValues[n1 + j + 1])^(n0 - j)
			)		
	}
	calculateMaximumUpperFDRBound <- function(criticalValues, n, startIDX_SUD) 
	{
		# Formulas are from Finner, Gontscharuk, Dickhaus: FDR controlling step-up-down tests
		# 	related to the asmptotically optimal rejection curve. (to appear)
		
		# Calculating the probability mass function of V under a DU(n_0, n) model
		# 	n 	= Number of hypotheses
		# 	n0 	= Number of true hypotheses
		# 	pm[j+1,n0+1] = P_{n,n0}(V = j)
		
		pm <- sapply( 1:n,
				function(n0) 
				{
					sapply(0:n, # actually we only had to go to n0, but in this way pm will be a matrix 
							function(j) 
							{
								pmf(criticalValues, startIDX_SUD, n, n0, j)								
							}
					)
				}
		)
		# now the special case n0 = 0, then P_{n,n0}(V=0) = 1
		pm <- cbind(c(1, rep(0, times = n)), pm)
		
		# just for plausibility, gonna look if P_{n,n0}(V in {0, ..., n}) = 1 for every n0 = 1, ..., n
		rng <- range(colSums(pm))
		if (rng[2]-rng[1] > 0.01)
			warning("Maximum upper bounds of FDR probably not accurate!")
		
		# Calculating b(n,n0|startIDX_SUD) for every n0
		# which is a uppper bound for the FDR according to Finner, Gontscharuk, Dickhaus.
		
		bn <- sapply(1:n, 
				function(n0) 
				{
					n1 <- n - n0 
					n0 * sum(sapply(1:n0, function(j) criticalValues[n1 + j]/(n1 + j) * pm[j, n0]))
				}
		)
		bn
		
		return(max(bn))		
	}
	SearchInitialBetaInterval <- function(n, startIDX_SUD, alpha, initialBeta) 
	{
		# searches two beta's: beta1 and beta2 such that the beta2-adjusted AORC
		# controls the FDR and the beta1-adjusted AORC not and beta2 + step = beta1
		
		criticalValues <- sapply(1:n, function(i) i * alpha / (n + initialBeta - i * (1 - alpha)))
		UpperFDRBound <- calculateMaximumUpperFDRBound(criticalValues, n, startIDX_SUD)					
		
		# startBeta controls the FDR, thus beta must be reduced! 
		if (UpperFDRBound < alpha)
		{
			FDRControlOfInitialBeta <- TRUE
			step <- -1
		}
		else
		{
			FDRControlOfInitialBeta <- FALSE
			step <- 1
		}
		
		beta <- initialBeta + step
		intervalFound <- FALSE
		while (!intervalFound) 
		{			
			criticalValues <- sapply(1:n, function(i) i * alpha / (n + beta - i * (1 - alpha)))
			UpperFDRBound <- calculateMaximumUpperFDRBound(criticalValues, n, startIDX_SUD)
			
			# if we have FDRControl by the initialBeta but not for beta, then we are done!
			# Also if we have not control of the FDR by the initalBeta but for beta, then we are done!
			if (xor(UpperFDRBound < alpha, FDRControlOfInitialBeta))
				intervalFound <- TRUE
			else
				# initialBeta and beta both control the FDR or both do not control the FDR
				beta <- beta + step
		}
		
		if (FDRControlOfInitialBeta)
			return(c(beta, beta + 1))
		
		return(c(beta - 1, beta))
	}
	#---------------------------   Subfunctions   -------------------------
	
	if (!silent)
		cat("Searching initial interval to start the bisection approach.\n")
	
	betaInt <- SearchInitialBetaInterval(n, startIDX_SUD, alpha, initialBeta)
	
	beta 			<- betaInt[2]
	step 			<- (betaInt[2] - betaInt[1]) / 2
	numberOfSteps 	<- 0
	
	if (!silent) 
		cat("Starting the bisection approach.\n")
	
	lastFeasibleBeta 			<- beta
	lastFeasibleUpperFDRBound 	<- -Inf 
	while(numberOfSteps < maxBinarySteps)
	{		
		numberOfSteps <- numberOfSteps + 1
		criticalValues <- sapply(1:n, function(i) i * alpha / (n + beta - i * (1 - alpha)))
		UpperFDRBound <- calculateMaximumUpperFDRBound(criticalValues, n, startIDX_SUD)
		
		if (!silent)
			cat("Step ", numberOfSteps, ": beta =", beta, " => Upper FDR bound =", UpperFDRBound, "\n")
		
		if (alpha - tolerance <= UpperFDRBound && UpperFDRBound <= alpha)
		{
			if (!silent)
			{
				cat("\nUpper FDR bound element in [alpha - tolerance, alpha]\n")
				cat("Returned beta =", beta, " => Upper FDR bound =", UpperFDRBound, "\n")
			}
			return(beta)
		}
		
		if (UpperFDRBound > alpha)
			beta <- beta + step
		else
		{	
			lastFeasibleBeta 			<- min(beta, lastFeasibleBeta)
			lastFeasibleUpperFDRBound 	<- max(UpperFDRBound, lastFeasibleUpperFDRBound) 
			beta <- beta - step
		}
		
		step <- step / 2			
	}	
	
	
	if(!silent)
		cat("\nReturned beta =", lastFeasibleBeta, " => Upper FDR bound =", lastFeasibleUpperFDRBound, "\n" )
	
	return(lastFeasibleBeta)
}

#------------------------ AORC---------------------#

#' Performs a step-up-down test on pValues. The critical values are based 
#' on the asymptotically optimal rejection curve. To have finite FDR control,
#' an automatic adjustment of the critical values is done (see
#' details for more).      
#' 
#' The graph of the function f(t) = t / (t * (1 - alpha) + alpha) is called the asymptotically
#' optimal rejection curve. Denote by finv(t) the inverse of f(t). Using the 
#' critical values finv(i/n) for i = 1, ..., n yields asymptotic FDR control.
#' To ensure finite control it is possible to adjust f(t) by a factor. The
#' function calculateBetaAdjustment() calculates a beta such that (1 + beta / n) * f(t)
#' can be used to control the FDR for a given finite sample size. If the 
#' parameter betaAdjustment is not provided, calculateBetaAdjustment() will be called automatically.
#' @title Step-up-down test based on the asymptotically optimal rejection curve.
#' @param pValues pValues to be used. They are assumed to be stochastically independent.
#' @param alpha The level at which the FDR shall be controlled.
#' @param startIDX_SUD The index at which critical value the 
#' 		step-up-down test starts. Default is length(pValues) and thus the function
#' 		aorc() by default behaves like an step-up test.  
#' @param betaAdjustment A numeric value to adjust the asymptotically optimal
#' 		rejection curve for the finite sample case. If betaAdjustment is not
#' 		set an algorithm will calculate it, but this can be time-consuming.  
#' @param silent If true any output on the console will be suppressed.
#' @return A list containing:
#' 
#'	\item{adjPValues}{A numeric vector containing the adjusted pValues}
#' 
#'	\item{rejected}{A logical vector indicating which hypotheses are rejected}
#' 
#'	\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test}
#' 
#' 	\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error (FDR)
#'     controlled by the function and the level \code{alpha}.}
#'  
#' @references Finner, H., Dickhaus, T. & Roters, M. (2009).
#' 		On the false discovery rate and an asymptotically optimal rejection curve.
#' 		The Annals of Statistics 37, 596-618.
#' @author MarselScheer
#' @export
#' @examples 
#' r <- c(runif(10), runif(10, 0, 0.01))
#' result <- aorc(r, 0.05)
#' result <- aorc(r, 0.05, startIDX_SUD = 1)  ## step-down
#' result <- aorc(r, 0.05, startIDX_SUD = length(r))  ## step-up
aorc <- function(pValues, alpha, startIDX_SUD = length(pValues), betaAdjustment, silent = FALSE) 
{	
	len <- length(pValues)
	if (missing(betaAdjustment))
	{
		if (!silent)
			cat("Using calculateBetaAdjustment() to set the missing parameter betaAdjustment.\n")
		betaAdjustment = calculateBetaAdjustment(len, startIDX_SUD, alpha, silent)
	}
	
	criticalValues <- sapply(1:len, function(i) i * alpha / (len + betaAdjustment - i * (1 - alpha))) 
	rejected <- SUD(pValues, criticalValues, startIDX_SUD)
	
	if (! silent)
	{
		cat("\n\n\t\tAsymptotically optimal rejection curve (2009)\n\n")
		printRejected(rejected, pValues)
	}
	
	return(list(rejected = rejected, criticalValues = criticalValues,
					errorControl = new(Class='ErrorControl',type="FDR",alpha=alpha)))
}

#' @export 
#' @nord
mutoss.aorc <- function() { return(new(Class="MutossMethod",
					label="Asymptotically optimal rejection curve (2009)",
					errorControl="FDR",
					callFunction="aorc",
					output=c("criticalValues", "rejected", "errorControl"),
					info="<h2>Step-up-down procedure based on the asymptotically optimal rejection curve</h2>\n\n\
							<p> The graph of the function f(t) = t / (t * (1 - alpha) + alpha) is called the asymptotically \
							optimal rejection curve. Denote by finv(t) the inverse of f(t). Using the \ 
							critical values finv(i/n) for i = 1, ..., n yields asymptotic FDR control. \
							To ensure finite FDR control it is possible to adjust f(t) by a factor. The \
							function calculateBetaAdjustment() calculates a beta such that (1 + beta / n) * f(t) \
							can be used to control the FDR for a given finite sample size. If beta is not provided,
							calculateBetaAdjustment() will be called automatically.\ 
							</p>\n\
							<h3>Reference:</h3>\
							<ul>\
							<li>Finner, H., Dickhaus, T. & Roters, M. \"<i> On the false discovery rate and an asymptotically \
							optimal rejection curve. </i>\" The Annals of Statistics 37, 596-618. </li>\n\
							</ul>",
					parameters=list(pValues=list(type="numeric"), alpha=list(type="numeric"), 
							startIDX_SUD=list(type="integer", label="Start Index for Step-Up-Down", optional=TRUE),
							betaAdjustment=list(type="numeric", label="Adjustment factor beta_n", optional=TRUE))
			)) }



#aorc( runif(30), 0.05, 5)

#----------------- Banjamini Liu----------------#

#' Benjamini-Liu's step-down procedure is applied to pValues. 
#' The procedure controls the FDR if the corresponding test statistics are stochastically independent.
#' 
#' The Benjamini-Liu (BL) step-down procedure neither dominates nor is dominated by the Benjamini-Hochberg (BH) step-up procedure.
#' However, in Benjamini and Liu (1999) a large simulation study concerning the power of the two procedures reveals that the BL step-down procedure is more suitable when the number of hypotheses is small. 
#' Moreover, if most hypotheses are far from the null then the BL step-down procedure is more powerful than the BH step-up method.
#' The BL step-down method calculates critical values according to Benjamin and Liu (1999), 
#' i.e., c_i = 1 - (1 - min(1, (m*alpha)/(m-i+1)))^(1/(m-i+1)) for i = 1,...,m, where m is the number of hypotheses tested. 
#' Then, let k be the smallest i for which P_(i) > c_i and reject the associated hypotheses H_(1),...,H_(k-1).
#' 
#' @title Benjamini-Liu (1999) step-down procedure
#' @param pValues Numeric vector of p-values 
#' @param alpha The level at which the FDR is to be controlled.
#' @param silent If true any output on the console will be suppressed.
#' @return A list containing:
#' 
#'	\item{adjPValues}{A numeric vector containing the adjusted pValues.}
#' 
#'  \item{criticalValues}{A numeric vector containing critical values used in the step-up-down test.} 
#' 
#'	\item{rejected}{A logical vector indicating which hypotheses are rejected.}
#' 
#' 	\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}
#' 
#' @author Werft Wiebke
#' @references Bejamini, Y. and Liu, W. (1999). A step-down multiple hypotheses testing procedure that controls the false discovery rate under independence. 
#' 				Journal of Statistical Planning and Inference Vol. 82(1-2): 163-170. 
#' @export
#' @examples 
#' alpha <- 0.05
#' p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9, max=1))
#' result <- BL(p, alpha)
#' result <- BL(p, alpha, silent=TRUE)
BL <- function(pValues, alpha, silent=FALSE) {
	m <- length(pValues)
	criticalValues <- sapply(1:m, function(i) 1-(1-min(1, (m*alpha)/(m-i+1)))^(1/(m-i+1)))
	rejected <- SD(pValues, criticalValues)
	index <- order(pValues) 	# index for sorting pValues
	rindex <- order(index) 		# reversed index to obtain the original order 
	spval <- pValues[index]
	adjPValues <- vector(mode="numeric",length=m)
	adjPValues[1] <- min(1 - (1 - spval[1])^m, 1)
	for (i in 2:m) adjPValues[i] <- max(adjPValues[i - 1], ifelse((alpha*m)/(m-i+1)<=1, ((m-i+1)/m)*(1 - (1 - spval[i])^(m - i + 1)), 0))#(0)?! 
	adjPValues <- adjPValues[rindex] # obtain the original order
	#rejected <- (adjustedPValues <= alpha) # either this or SUD leads to rejected 
	if (! silent)
	{
		cat("\n\n\t\tBenjamini-Liu's (1999) step-down procedure\n\n")
		printRejected(rejected, pValues, adjPValues)
	}
	return(list(adjPValues=adjPValues, criticalValues=criticalValues, rejected=rejected,
					errorControl = new(Class='ErrorControl',type="FDR",alpha=alpha)))
}
#' @export
#' @nord
mutoss.BL <- function() { return(new(Class="MutossMethod",
					label="Benjamini-Liu (1999) step-down",
					errorControl="FDR",
					callFunction="BL",
					output=c("adjPValues", "criticalValues", "rejected", "errorControl"),
					assumptions=c("Independent test statistics."),
					info="<h2>Benjamini-Liu (1999) step-down </h2>\n\n
							<h3>Reference:</h3>
							<ul>
							<li>Bejamini, Y. and Liu, W. (1999). \"<i> A step-down multiple hypotheses testing procedure that controls the false discovery rate under independence . </i>\" Journal of Statistical Planning and Inference Vol. 82(1-2): 163-170. </li>\n
							</ul>
							<p>Benjamini-Liu's step-down procedure is applied to pValues. 
							The procedure controls the FDR if the corresponding test statistics are stochastically independent. 
							In Benjamini and Liu (1999) a large simulation study concerning the power of the two procedures suggested that the BL step-down procedure is more powerfull then the Linear Step-Up (BH) when the number of hypotheses is small. 
							This is also the case when most hypotheses are far from the null. The BL step-down method calculates critical values according to Benjamin and Liu (1999), i.e.
							<i>c<sub>i</sub> = 1 - (1 - min(1, m*&alpha;/(m-i+1)))<sup>(1/(m-i+1))</sup></i> for <i>i = 1,...,m</i>, 
							where <i>m</i> is the number of hypotheses tested. 
							Then, let <i>k</i> be the smallest <i>i</i> for which <i>P<sub>(i)</sub> > c<sub>i</sub></i> and reject associated hypotheses <i>H<sub>(1)</sub>,...,H<sub>(k-1)</sub></i>.",
					parameters=list(pValues=list(type="numeric"), alpha=list(type="numeric"))
			)) }

#-------------------- BH Linear Step Up--------------------#

#' Benjamini-Hochbergs Linear Step-Up Procedure. 
#' The procedure controls the FDR when the test statistics are stochastically independent or satisfy positive regression dependency (PRDS) (see Benjamini and Yekutieli 2001 for details).
#' The Benjamini-Hochberg (BH) step-up procedure considers ordered pValues P_(i). 
#' It defines k as the largest i for which P_(i) <= i*alpha/m and then 
#' rejects all associated hypotheses H_(i) for i=1,...,k. In their seminal paper, Benjamini and Hochberg (1995) show 
#' that for 0 <= m_0 <= m independent pValues corresponding to true null hypotheses 
#' and for any joint distribution of the m_1 = m-m_0 p-values corresponding to the 
#' non null hypotheses, the FDR is controlled at level (m_0/m)*alpha.  Under the
#' assumption of the PRDS property, (for details see Benjamini and Yekutieli 2001). 
#' In Benjamini et al. (2006) the BH procedure is improved by adaptive procedures
#' which use an estimate of m_0 and apply the BH method al level alpha'=alpha*m/m_0, to 
#' fully exhaust the desired level alpha (see Adaptive Benjamini Hochberg and Two Stage Banjamini Yekutieli).        
#' @title Benjamini-Hochberg (1995) linear step-up procedure
#' @param pValues The used unadjusted pValues.
#' @param alpha The level at which the FDR shall be controlled.
#' @param silent If true any output on the console will be suppressed. 
#' @return A list containing:
#' 
#'	\item{adjPValues}{A numeric vector containing the adjusted pValues}
#' 
#' 	\item{criticalValues}{A numeric vector containing critical values used in the step-up test} 
#'	
#' 	\item{rejected}{A logical vector indicating which hypotheses are rejected}
#' 
#' 	\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}
#' 	
#' @author Werft Wiebke
#' @references 	Benjamini, Y. and Hochberg, Y. (1995). Controlling the false discovery rate: A practical and powerful approach to mulitple testing.
#'					 Journal of the Royal Statistical Society, Series B, 57:289-300.\eqn{n}
#' 
#' 				Benjamini, Y. and Yekutieli, D. (2001). The control of the false discovery rate in multiple testing under dependency.
#' 				 Annals of Statistics, 29(4):1165-1188.\eqn{n}
#' 
#' 				Benjamini, Y., Krieger, A. and Yekutieli, D. (2006). Adaptive linear step-up procedures that control the false
#' 							discovery rate. Biometrika, 93(3):491-507.
#' @export
#' @examples 
#' alpha <- 0.05
#' p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9, max=1))
#' result <- BH(p, alpha)
#' result <- BH(p, alpha, silent=TRUE)
BH <- function(pValues, alpha, silent=FALSE) {
	m <- length(pValues)
	criticalValues <- sapply(1:m, function(i) (i*alpha)/m)
	adjPValues <- p.adjust(pValues, "BH")
	rejected <- (adjPValues <= alpha)
	if (! silent)
	{
		cat("\n\n\t\tBenjamini-Hochberg's (1995) step-up procedure\n\n")
		printRejected(rejected, pValues, adjPValues)
	}
	return(list(
					adjPValues=adjPValues, 
					criticalValues=criticalValues,
					rejected=rejected,
					errorControl = new(Class='ErrorControl',type="FDR",alpha=alpha)))
}
#' @export
#' @nord
mutoss.BH <- function() { return(new(Class="MutossMethod",
					label="Benjamini-Hochberg (1995) step-up",
					errorControl="FDR",
					callFunction="BH",
					output=c("adjPValues", "criticalValues", "rejected", "errorControl"),
					assumptions=c("independence or positive regression dependency"),
					info="<h2>Benjamini-Hochberg (1995) Linear Step-Up Procedure </h2>\n\n\
							<p>The Benjamini-Hochberg (BH) linear step-up procedure controls the FDR if the test statistics are stochastically independent or satisfy positive regression dependency  (see Benjamini and Yekutieli 2001 for details).
							In their seminal paper, Benjamini and Hochberg (1995) suggest the False Discovery Rate (FDR) as an alternative error criterion to the Family-Wise-Error-Rate and show that for <i>0<=m<sub>0</sub><=m</i> independent pValues corresponding to true null hypotheses 
							and for any joint distribution of the <i>m<sub>1</sub>=m-m<sub>0</sub></i> p-values corresponding to the non-null hypotheses the FDR is controlled at level <i>(m<sub>0</sub>/m)*&alpha;</i>.  
							Benjamini and Yekutieli show (2001) that this procedure controls the FDR in a much more general setting i.e. when the PRDS condition is satisfied. 
							#####Benjamini et al. (2006) improved by adaptive procedures which use an estimate of <i>m<sub>0</sub></i> and apply the BH method at level <i>&alpha;'=&alpha*m/m<sub>0</sub></i>, to fully exhaust the desired level <i>&alpha;</i>.</p>\n
							<h3>References:</h3>\
							<ul>\
							<li>Benjamini, Y. and Hochberg, Y. (1995). \"<i> Controlling the false discovery rate: A practical and powerful approach to mulitple testing. </i>\" Journal of the Royal Statistical Society, Series B, 57:289-300. </li>\n\
							<li>Benjamini, Y. and Yekutieli, D. (2001). \"<i> The control of the false discovery rate in multiple testing under dependency. </i>\" Annals of Statistics, 29(4):1165-1188. </li>\n\
							<li>Benjamini, Y., Krieger, A. and Yekutieli, D. \"<i> Adaptive linear step-up procedures that control the false
							discovery rate. </i>\" Biometrika, 93(3):491-507, 2006. </li>\n\
							</ul>",
					parameters=list(pValues=list(type="numeric"), alpha=list(type="numeric"))
			)) }


#' The Benjamini-Yekutieli step-up procedure is applied to pValues. 
#' The procedure ensures FDR control for any dependency structure.
#' 
#' The critical values of the Benjamini-Yekutieli (BY) procedure are calculated by 
#' replacing the alpha of the Benjamini-Hochberg procedure by alpha/sum(1/1:m)), i.e.,
#' c(i)=i*alpha/(m*(sum(1/1:m))) for i=1,...,m. For large number m of hypotheses the critical values of the BY procedure and the 
#' BH procedure differ by a factor log(m). Benjamini and Yekutieli (2001) showed that this step-up procedure controls
#' the FDR at level alpha*m/m0 for any dependency structure among the test statistics.
#' 
#' @title Benjamini-Yekutieli (2001) step-up procedure
#' @param pValues The used unadjusted pValues.
#' @param alpha The level at which the FDR shall be controlled.
#' @param silent If true any output on the console will be suppressed. 
#' @return A list containing:
#' 
#' \item{adjPValues}{A numeric vector containing the adjusted pValues}
#' 
#' \item{criticalValues}{A numeric vector containing critical values used in the step-up-down test} 
#' 
#' \item{rejected}{A logical vector indicating which hypotheses are rejected}
#' 
#' 	\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}
#' 
#' @author WerftWiebke
#' @references 	Benjamini, Y. and Yekutieli, D. (2001). The control of the false discovery rate in multiple testing under dependency.
#' 				 Annals of Statistics, 29(4):1165-1188. 
#' @export
#' @examples 
#' alpha <- 0.05
#' p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9, max=1))
#' result <- BY(p, alpha)
#' result <- BY(p, alpha, silent=TRUE)
BY <- function(pValues, alpha, silent=FALSE) {
	m <- length(pValues)
	a <- sum(1/(1:m))
	criticalValues <- sapply(1:m, function(i) (i*alpha)/(a*m))
	#rejected <- SU(pValues, criticalValues)
	adjPValues <- p.adjust(pValues, "BY")
	rejected <- (adjPValues <= alpha)
	if (! silent)
	{
		cat("\n\n\t\tBenjamini-Yekutieli's (2001) step-up procedure\n\n")
		printRejected(rejected, pValues, adjPValues)
	}
	return(list(adjPValues=adjPValues, criticalValues=criticalValues, rejected=rejected,
					errorControl = new(Class='ErrorControl',type="FDR",alpha=alpha)))
}
#' @export
#' @nord
mutoss.BY <- function() { return(new(Class="MutossMethod",
					label="Benjamini-Yekutieli (2001) step-up",
					errorControl="FDR",
					callFunction="BY",
					output=c("adjPValues", "criticalValues", "rejected", "errorControl"),
					assumptions=c("any dependency structure"),
					info="<h2>Benjamini-Yekutieli (2001) step-up procedure </h2>\n\n\
							<h3>Reference:</h3>\
							<ul>\
							<li>Bejamini, Y. and Yekutieli, D. (2001). \"<i> The control of the false discovery rate in multiple testing under dependency. </i>\" Annals of Statistics, 29(4):1165-1188. </li>\n\
							</ul>
							<p>The Benjamini-Yekutieli step-up procedure is applied to pValues. 
							The procedure ensures FDR control for any dependency structure. 
							The critical values of the Benjamini-Yekutieli (BY) procedure are calculated by 
							replacing the <i>&alpha;</i> of the Benjamini-Hochberg procedure by <i>&alpha;/(&sum;1/i)</i>, i.e.
							<i>c(i)=i*&alpha;/m*(&sum;1/i)</i> for <i>i=1,...,m</i>. For large number <i>m</i> of hypotheses the critical values of the BY procedure and the 
							BH procedure differ by a factor <i>log(m)</i>. Benjamini and Yekutieli (2001) showed that this step-up procedure controls
							the FDR at level <i>&alpha;*m/m<sub>0</sub></i> for any test statistics dependency structure.</p>\n",
					parameters=list(pValues=list(type="numeric"), alpha=list(type="numeric"))
			)) }

#' The Hochberg step-up procedure is based on marginal p-values. It controls the FWER in the strong 
#' sense under joint null distributions of the test statistics that satisfy Simes' inequality.
#' 
#' The Hochberg procedure is more powerful than Holm's (1979) procedure, but the test statistics need to be
#' independent or have a distribution with multivariate total positivity of order two or a scale mixture
#' thereof for its validity (Sarkar, 1998).
#' Both procedures use the same set of critical values c(i)=alpha/(m-i+1). Whereas Holm's procedure is a step-down 
#' version of the Bonferroni test, and Hochberg's is a step-up version of the Bonferroni test.
#' Note that Holm's method is based on the Bonferroni inequality and is valid regardless of the joint
#' distribution of the test statistics.
#' @title Hochberg (1988) step-up procedure
#' @param pValues The used raw pValues.
#' @param alpha The level at which the FDR shall be controlled.
#' @param silent If true any output on the console will be suppressed. 
#' @return A list containing:
#' 
#'	\item{adjPValues}{A numeric vector containing the adjusted pValues}
#' 
#'	\item{rejected}{A logical vector indicating which hypotheses are rejected}
#' 
#'	\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test}
#' 
#' 	\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}
#'  
#' @author WerftWiebke
#' @references 	Hochberg, Y. (1988). A sharper Bonferroni procedure for multiple tests of significance. 
#' 		Biometrika, 75:800-802.\eqn{n}
#' 		
#' 		Huang, Y. and Hsu, J. (2007). Hochberg's step-up method: cutting corners off Holm's step-down method. Biometrika, 94(4):965-975.
#' @export
#' @examples 
#' alpha <- 0.05
#' p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9,max=1))
#' result <- hochberg(p, alpha)
#' result <- hochberg(p, alpha, silent=TRUE)
hochberg <- function(pValues, alpha, silent=FALSE) {
	m <- length(pValues)
	criticalValues <- sapply(1:m, function(i) alpha/(m-i+1))
	#rejected <- SU(pValues, criticalValues)
	adjPValues <- p.adjust(pValues, "hochberg")
	rejected <- (adjPValues <= alpha)
	if (! silent)
	{
		cat("\n\n\t\tHochberg's (1988) step-up procedure\n\n")
		printRejected(rejected, pValues, adjPValues)
	}
	return(list(adjPValues=adjPValues, criticalValues=criticalValues, rejected=rejected,
					errorControl = new(Class='ErrorControl',type="FWER",alpha=alpha)))
}
#' @export
#' @nord
mutoss.hochberg <- function() { return(new(Class="MutossMethod",
					label="Hochberg (1988) step-up",
					errorControl="FWER",
					callFunction="hochberg",
					output=c("adjPValues", "criticalValues", "rejected", "errorControl"),
					assumptions=c("independent tests"),
					info="<h2>Hochberg (1988) step-up procedure </h2>\n\n\
							<h3>Reference:</h3>\
							<ul>\
							<li>Hochberg, Y. (1988). <i> A sharper Bonferroni procedure for multiple tests of significance. </i> Biometrika, 75:800-802. </li>\n\
							<li>Huang, Y. and Hsu, J. (2007). <i> Hochberg's step-up method: cutting corners off Holm's step-down method. </i>Biometrika, 94(4):965-975.</li>\n							
							</ul>
							<p>The Hochberg step-up procedure is based on marginal p-values. It controls the FWER in the strong 
							sense under joint null distributions of the test statistics that satisfy Simes' inequality.
							The Hochberg procedure is more powerful than Holm's (1979) procedure, but the test statistics need to be
							independent or have a distribution with multivariate total positivity of order two or a scale mixture
							thereof for its validity (Sarkar, 1998). 
							Both procedures use the same set of critical values <i>c(i)=&alpha;/(m-i+1)</i>. Whereas Holm's procedure is a step-down 
							version of the Bonferroni test, and Hochberg's is a step-up version of the Bonferroni test.
							Note that Holm's method is based on the Bonferroni inequality and is valid regardless of the joint
							distribution of the test statistics.</p>\n",
					parameters=list(pValues=list(type="numeric"), alpha=list(type="numeric"))
			)) }


#' The adaptive Benjamini-Hochberg step-up procedure is applied to pValues.
#' It controls the FDR at level alpha for independent or positive regression dependent test statistics.
#' 
#' In the adaptive Benjamini-Hochberg step-up procedure the number of true null hypotheses is estimated first as in Hochberg and
#' Benjamini (1990), and this estimate is used in the procedure of Benjamini and
#' Hochberg (1995) with alpha'=alpha*m/m0. 
#' @title Benjamini-Hochberg (2000) adaptive linear step-up procedure
#' @param pValues The used raw pValues.
#' @param alpha The level at which the FDR shall be controlled.
#' @param silent If true any output on the console will be suppressed. 
#' @return A list containing:
#' 
#'	\item{adjPValues}{A numeric vector containing the adjusted pValues}
#' 
#' \item{criticalValues}{A numeric vector containing critical values used in the step-up-down test} 
#' 
#'	\item{rejected}{A logical vector indicating which hypotheses are rejected}
#' 
#' \item{pi0}{An estimate of the proportion of true null hypotheses among all hypotheses (pi0=m0/m). }
#' 
#' \item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}
#' 
#' @author WerftWiebke
#' @references 	Benjamini, Y. and Hochberg, Y. (2000). On the Adaptive Control of the False Discovery Rate
#' 							in Multiple Testing With Independent Statistics.
#' 							Journal of Educational and Behavioral Statistics, 25(1): 60-83.\eqn{n}
#' 
#' 							Hochberg, Y. and Benjamini, Y. (1990). More powerful procedures for multiple significance testing. 
#' 							Statistics in Medicine 9, 811-818.\eqn{n}
#' 
#' 							Benjamini, Y. and Hochberg, Y. (1995). Controlling the false discovery rate: A practical and powerful approach to mulitple testing.
#'							 Journal of the Royal Statistical Society, Series B, 57:289-300.
#' @export
#' @examples 
#' alpha <- 0.05
#' p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9, max=1))
#' result <- adaptiveBH(p, alpha)
#' result <- adaptiveBH(p, alpha, silent=TRUE)
adaptiveBH <- function(pValues, alpha, silent=FALSE) {
	m <- length(pValues)
	pi0.ABH <- ABH_pi0_est(pValues)$pi0
	criticalValues <- sapply(1:m, function(i) (i*alpha)/(m*pi0.ABH))
	adjPValues <- p.adjust(pValues,"BH")*pi0.ABH
	rejected <- (adjPValues <= alpha)
	if (! silent)
	{
		cat("\n\n\t\tBenjamini-Hochberg (2000) adaptive step-up procedure\n\n")
		printRejected(rejected, pValues, adjPValues)
	}
	return(list(adjPValues=adjPValues, criticalValues=criticalValues, rejected=rejected, pi0=pi0.ABH,
					errorControl = new(Class='ErrorControl',type="FDR",alpha=alpha)))
}
#' @export
#' @nord
mutoss.adaptiveBH <- function() { return(new(Class="MutossMethod",
					label="Benjamini-Hochberg (2000) adaptive step-up",
					errorControl="FDR",
					callFunction="adaptiveBH",
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


#---------------------- Adaptive STS-----------------#

#' Storey-Taylor-Siegmund's (2004) adaptive step-up procedure 
#' 
#' The adaptive STS procedure uses a conservative estimate of pi0 which is 
#' plugged in a linear step-up procedure. The estimation of pi0 requires a 
#' parameter (lambda) which is set to 0.5 by default.
#' Note that the estimated pi0 is truncated at 1 as suggested by the author, 
#' so the implemetation of the procedure is not entirely supported by the proof in the reference.
#' @title Storey-Taylor-Siegmund (2004) adaptive step-up procedure
#' @param pValues The used raw pValues.
#' @param alpha The level at which the FDR shall be controlled.
#' @param lambda The tuning parameter for the estimation procedure (defaults to 0.5)
#' @param silent If true any output on the console will be suppressed. 
#' @return A list containing:
#' 
#'	\item{adjPValues}{A numeric vector containing the adjusted pValues}
#' 
#'	\item{rejected}{A logical vector indicating which hypotheses are rejected}
#' 
#'	\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test}
#' 
#' 	\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}
#'  
#' @author Werft Wiebke
#' @references Storey, J.D., Taylor, J.E. and Siegmund, D. (2004). Strong control, conservative point estimation and
#' 							simultaneous conservative consistency of false discovery rates: a unified approach.
#' 		Journal of the Royal Statistical Society, B 66(1):187-205.
#' @examples 
#' alpha <- 0.05
#' p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9,max=1))
#' result <- adaptiveSTS(p, alpha, lambda=0.5)
#' result <- adaptiveSTS(p, alpha, lambda=0.5, silent=TRUE)
#' @export
adaptiveSTS <- function(pValues, alpha, lambda=0.5, silent=FALSE) {
	m <- length(pValues)
	adjP <- p.adjust(pValues,"BH")
	pi0 <- storey_pi0_est(pValues, lambda)$pi0
	criticalValues <- sapply(1:m, function(i) (i*alpha)/(m*pi0))
	adjPValues <- adjP*min(pi0, 1)
	rejected <- (adjPValues <= alpha)
	if (! silent)
	{
		cat("\n\n\t\tStorey-Taylor-Siegmund (2004) adaptive step-up procedure\n\n")
		printRejected(rejected, pValues, adjPValues)
	}
	return(list(adjPValues=adjPValues, criticalValues=criticalValues, rejected=rejected, pi0=pi0,
					errorControl = new(Class='ErrorControl',type="FDR",alpha=alpha)))
}
#' @export
#' @nord
mutoss.adaptiveSTS <- function() { return(new(Class="MutossMethod",
					label="Storey-Taylor-Siegmund (2004) adaptive step-up",
					errorControl="FDR",
					callFunction="adaptiveSTS",
					output=c("adjPValues", "criticalValues", "rejected", "pi0", "errorControl"),
					assumptions=c("test independence or positive regression dependency"),
					info="<h2>Storey-Taylor-Siegmund (2004) adaptive step-up procedure</h2>\n\n
							
							<p> The adaptive STS method uses a conservative estimate of <i>pi0</i> which is plugged in a linear step-up procedure. The estimation of <i>pi0</i> requires a parameter <i>&lambda;</i> which is set to <i>0.5</i> by default.
							Note that the estimated <i>pi0</i> is truncated at 1 as suggested by the author, so the implemetation of the procedure is not entirely supported by the proof in the reference. 
							</p>\n
							<h3>Reference:</h3>
							<ul>
							<li>Storey, J.D., Taylor, J.E. and Siegmund, D. \"<i> Strong control, conservative point estimation and
							simultaneous conservative consistency of false discovery rates: a unified approach.</i>\" Journal of the Royal Statistical Society, B 66(1):187-205, 2004. </li>\n
							</ul>",
					parameters=list(pValues=list(type="numeric"), alpha=list(type="numeric"), lambda=list(type="numeric", default = 0.5))
			)) }

#---------------------------- Sidack Step Down--------------------------------#
#' The Sidak-like (1987) step-down procedure is applied to pValues
#' The Sidak-like step-down procedure is an improvement over the Holm's (1979) 
#' step-down procedure. The improvement is analogous to Sidak's correction 
#' over the original Bonferroni procedure. This Sidak-like step-down procedure 
#' assumes positive orthant dependent test statistics.
#' @title Sidak-like (1987) step-down procedure
#' @param pValues The used raw pValues.
#' @param alpha The level at which the FWER shall be controlled.
#' @param silent If true any output on the console will be suppressed. 
#' @return A list containing:
#' 
#'	\item{adjPValues}{a numeric vector containing the adjusted pValues}
#' 
#'	\item{rejected}{a logical vector indicating which hypotheses are rejected}
#' 
#'	\item{criticalValues}{a numeric vector containing critical values used in the step-up-down test}
#' 
#' 	\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}
#' 
#' @author WerftWiebke
#' @references 	Hollander, B.S. and Covenhaver, M.D. (1987). An Improved Sequentially Rejective Bonferroni Test Procedure.
#' 		Biometrics, 43(2):417-423, 1987.
#' @examples 
#' alpha <- 0.05
#' p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9,max=1))
#' result <- SidakSD(p, alpha)
#' result <- SidakSD(p, alpha, silent=TRUE)
#' @export
SidakSD <- function(pValues, alpha, silent=FALSE) {
	require(multtest)
	m <- length(pValues)
	criticalValues <- sapply(1:m, function(i) 1-(1-alpha)^(1/(m-i+1)))
	#rejected <- SD(pValues, criticalValues)
	tmp <- mt.rawp2adjp(pValues, "SidakSD")
	adjPValues <- tmp$adjp[order(tmp$index),2]
	rejected <- (adjPValues <= alpha)
	if (! silent)
	{
		cat("\n\n\tSidak-like (1987) step-down procedure\n\n")
		printRejected(rejected, pValues, NULL)
	}
	return(list(adjPValues=adjPValues, criticalValues=criticalValues, rejected=rejected,
					errorControl = new(Class='ErrorControl',type="FWER",alpha=alpha)))
}
#' @export
#' @nord
mutoss.SidakSD <- function() { return(new(Class="MutossMethod",
					label="Sidak-like (1987) step-down",
					errorControl="FWER",
					callFunction="SidakSD",
					output=c("adjPValues", "criticalValues", "rejected", "errorControl"),
					assumptions=c("test independence","positive orthant dependent test statistics"),
					info="<h2>Sidak-like (1987) step-down procedure </h2>\n\n\
							<h3>Reference:</h3>\
							<ul>\
							<li>Hollander, B.S. and Covenhaver, M.D. \"<i> An Improved Sequentially Rejective Bonferroni Test Procedure.</i>\" Biometrics, 43(2):417-423, 1987. </li>\n\
							</ul>
							<p> The Sidak-like step-down procedure is an improvement over Holm's (1979) step-down procedure. The improvement is analogous to the Sidak's correction over the original Bonferroni procedure. This Sidak-like step-down procedure assumes positive orthant dependent test statistics. </p>\n",
					parameters=list(pValues=list(type="numeric"), alpha=list(type="numeric"))
			)) }

#----------------------------------Blanchard Roquain 2008 ----------------------#

#' Blanchard-Roquain (2009) 1-stage adaptive step-up
#' 
#' This is a step-up procedure with critical values
#' 
#' C_i = alpha * min( i * ( 1 - lambda * alpha) / (m - i + 1) , lambda )
#' 
#' where alpha is the level at which FDR should be controlled and lambda an 
#' arbitrary parameter belonging to (0, 1/alpha) with default value 1.
#' This procedure controls FDR at the desired level when the p-values are independent.
#' 
#' 
#' @title Blanchard-Roquain (2009) 1-stage adaptive step-up
#' @param pValues the used p-values (assumed to be independent)
#' @param alpha the level at which the FDR should be controlled. 
#' @param lambda parameter of the procedure, should belong to
#' (0, 1/alpha) (lambda=1 default)
#' @param silent if true any output on the console will be suppressed. 
#' @return A list containing:
#' 
#'	\item{rejected}{A logical vector indicating which hypotheses are rejected}
#' 
#'	\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test}
#' 
#' 	\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}
#'  
#' @author GillesBlanchard
#' @references Blanchard, G. and Roquain, E. (2009) 
#'   				Adaptive False Discovery Rate Control under Independence and Dependence
#' 		            Journal of Machine Learning Research 10:2837-2871. 
#' @export
indepBR <- function(pValues, alpha, lambda=1, silent = FALSE) 
{	
	if ( lambda <= 0 || lambda >= 1/alpha) {
		stop('indepBR() : lambda should belong to (0, 1/alpha)')
	}
	
	len <- length(pValues)
	
	criticalValues <- sapply( 1:len, function(i) alpha * min( i * ( 1 - lambda * alpha) / (len - i + 1) , lambda ) )
	rejected <- SU(pValues, criticalValues)
	
	if (! silent)
	{
		cat("\n\n\t\t Blanchard-Roquain 1-stage step-up under independence (2009)\n\n")
		printRejected(rejected, pValues)
	}
	
	return(list(rejected = rejected, criticalValues = criticalValues,
					errorControl = new(Class='ErrorControl',type="FDR",alpha=alpha)))
}
#' @export
#' @nord
mutoss.indepBR <- function() { return(new(Class="MutossMethod",
					label="Blanchard-Roquain adaptive step-up (2009)",
					errorControl="FDR",
					callFunction="indepBR",
					output=c("criticalValues", "rejected", "errorControl"),
					assumptions=c("p-value independence"),
					info="<h2> Blanchard-Roquain (2009) 1-stage adaptive step-up </h2>\n\n\
							<h3>Reference:</h3>\
							<ul>\
							<li>Blanchard, G. and Roquain, E. \"<i> Adaptive False Discovery Rate Control under Independence and Dependence.</i>\" 
							Journal of Machine Learning Research 10:2837-2871, 2009. . </li>\n\
							</ul>
							<p>This is a step-up procedure with critical values\n\ 
						 C<sub>i</sub> = alpha * min( i * ( 1 - lambda * alpha) / (m - i + 1) , lambda )\n\
						 where alpha is the level at which FDR should be controlled and lambda an \ 
							arbitrary parameter belonging to (0, 1/alpha) with default value 1. \
					This procedure controls FDR at the desired level when the p-values are independent.</p>",
					parameters=list(pValues=list(type="numeric"), alpha=list(type="numeric"), lambda=list(type="numeric", default=1))
			)) }


#----------------------------Blanchard Roquain 2009---------------------#
#' Blanchard-Roquain (2009) 2-stage adaptive step-up
#' 
#' This is an adaptive linear step-up procedure where the proportion of true
#' nulls is estimated using the Blanchard-Roquain 1-stage procedure with parameter lambda,
#' via the formula
#' 
#' estimated pi_0 = ( m - R(alpha,lambda)  + 1) / ( m*( 1 - lambda * alpha ) )
#' 
#' where R(alpha,lambda) is the number of hypotheses rejected by the BR 1-stage procedure,
#' alpha is the level at which FDR should be controlled and lambda an 
#' arbitrary parameter belonging to (0, 1/alpha) with default value 1.
#' This procedure controls FDR at the desired level when the p-values are independent.
#' 
#' @param pValues the used p-values (assumed to be independent)
#' @param alpha the level at which the FDR should be controlled. 
#' @param lambda parameter of the procedure, should belong to
#' (0, 1/alpha) (lambda=1 default)
#' @param silent if true any output on the console will be suppressed. 
#' @return A list containing:
#' 
#'	\item{rejected}{A logical vector indicating which hypotheses are rejected}
#' 
#' 	\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}
#' 
#' @author GillesBlanchard
#' @references Blanchard, G. and Roquain, E. (2009) 
#'   				Adaptive False Discovery Rate Control under Independence and Dependence
#' 		            Journal of Machine Learning Research 10:2837-2871. 
#' @export
twostageBR <- function(pValues, alpha, lambda=1, silent = FALSE) 
{	
	if ( lambda <= 0 || lambda >= 1/alpha) {
			stop('twostageBR() : lambda should belong to (0, 1/alpha)')
		}
	
	m <- length(pValues)
	stage1 <- indepBR( pValues, alpha, lambda, silent = TRUE)
	pi0inv <- ( 1 - lambda*alpha )*m / ( m + 1 - sum(stage1$rejected) )
	
	BHadjPValues <- p.adjust(pValues,"BH")
	
	rejected <- ( BHadjPValues <= alpha*pi0inv )
	if (! silent)
	{
		cat("\n\n\t\tBlanchard-Roquain (2009) 2-stage step-up Procedure\n\n")
		printRejected(rejected, pValues)
	}
	return(list(rejected=rejected, errorControl = new(Class='ErrorControl',type="FDR",alpha=alpha)))
	
}
#' @export
#' @nord
mutoss.twostageBR <- function() { return(new(Class="MutossMethod",
					label="Blanchard-Roquain 2-stage adaptive step-up (2009)",
					errorControl="FDR",
					callFunction="twostageBR",
					output=c("rejected", "errorControl"),
					assumptions=c("p-value independence"),
					info="<h2> Blanchard-Roquain 2-stage step-up under independence </h2>\n\n\
							<h3>Reference:</h3>\
							<ul>\
							<li>Blanchard, G. and Roquain, E. \"<i> Adaptive False Discovery Rate Control under Independence and Dependence.</i>\" 
							Journal of Machine Learning Research 10:2837-2871, 2009. . </li>\n\
							</ul>\
							<p>This is an adaptive linear step-up procedure where the proportion of true\
							 nulls is estimated using the Blanchard-Roquain 1-stage procedure with parameter lambda,\
                             via the formula\n\
 
				 estimated pi<sub>0</sub> = ( m - R(alpha,lambda)  + 1) / ( m*( 1 - lambda * alpha ) )\n\
 
				 where R(alpha,lambda) is the number of hypotheses rejected by the BR 1-stage procedure,
				 alpha is the level at which FDR should be controlled and lambda an 
 				 arbitrary parameter belonging to (0, 1/alpha) with default value 1.
 				 This procedure controls FDR at the desired level when the p-values are independent.</p>",
					parameters=list(pValues=list(type="numeric"), alpha=list(type="numeric"), lambda=list(type="numeric",default=1))
			)) }

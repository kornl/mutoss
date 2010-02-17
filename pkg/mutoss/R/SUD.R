# Implements the elementary functions for a general
# step-up-down test. 
# Step-Up and step-down are derived from that.
# 
# Author: MarselScheer
###############################################################################


#' A general step-up-down procedure.
#' 
#' Suppose we have n pValues and they are already sorted.
#' The procedure compares pValues[startIDX_SUD] with criticalValues[startIDX_SUD]
#' and then proceeds in step-up or step-down manner,   
#' depending on the result of this initial comparision.
#' 
#' If pValues[startIDX_SUD] <= criticalValues[startIDX_SUD], then the procedure
#' rejects the hypotheses associated with pValues[1], ..., pValues[startIDX_SUD]  
#' and carries on in a step-down manner from startIDX_SUD to n to reject additional hypotheses.
#' 
#' If pValues[startIDX_SUD] > criticalValues[startIDX_SUD], then the procedure
#' retains hypotheses associated with pValues[startIDX_SUD], ..., pValues[n] and 
#' carries on in a step-up manner with pValues[startIDX_SUD - 1], ..., pValues[1]. 
#' 
#' If startIDX_SUD equals n the algorithm behaves like a step-up procedure.
#' 
#' If startIDX_SUD equals 1 the algorithm behaves like a step-down procedure.
#' @param pValues pValues to be used.
#' @param criticalValues criticalValues for the step-up-down procedure
#' @param startIDX_SUD the index (between 1 and length(pValues)) used for the first 
#' 		comparison of pValues[startIDX_SUD] and criticalValues[startIDX_SUD]. Depending
#' 		on the result of this comparison the algorithm decides to proceed in 
#' 		step-up or step-down manner. 
#' @return rejected logical vector indicating if hypotheses are rejected or retained.
#' @author MuToss-Coding Team
#' @export
SUD <- function(pValues, criticalValues, startIDX_SUD) 
{		
	len <- length(criticalValues)
	
	# +++++++++++++++++   Plausis    ++++++++++++++
	if (len == 1)
		stop("SUD(): There is only 1 critical Value. Use the function SS()!")
	
	if (len != length(pValues))
		stop("SUD(): Length of critical Values and pValues need to be of the same length!")
	
	if (startIDX_SUD < 1 || len < startIDX_SUD )
		stop("SUD(): startIDX out of bound. criticalValues[startIDX] will not exist.")		
	# -----------------   Plausis    ---------------
	
	rejected <- rep(FALSE, times = len)

	# need to work with orderd pValues
	sortedPV <- sort(pValues, index.return = TRUE)
	
	suspiciousPV <- (sortedPV$x <= criticalValues)
	
	if (suspiciousPV[startIDX_SUD])
	{# Suspicious pValue.		
		# Actually doing now a StepDown on startIDX:len.
		# Additionally reject anything before startIDX
			
		nonSuspAboveStartIDX <- which(!suspiciousPV[startIDX_SUD:len]) 
		# ! looking only at the subset startIDX:len probably gives a shift of the index !
		# ! gonna correct this soon !
		
		# perhaps any pValue from startIDX to the end is suspicious, thus reject all!
		if (length(nonSuspAboveStartIDX) == 0)
			return(rep(TRUE, times = len))		
		
		# Correcting the shift
		nonSuspAboveStartIDX <- nonSuspAboveStartIDX + startIDX_SUD - 1
		
		# There must be some pValue between startIDX and the end that is not suspicious
		# Searching the first one. Anything immediately BEFORE that pValue will be rejected.		
		minIDX <- min(nonSuspAboveStartIDX) - 1
		
		rejected[sortedPV$ix[1:minIDX]] <- TRUE
	}
	else
	{# not suspicious pValue
		# Actually doing now a StepUp on 1:startIDX
		# The rejected are only the one rejected by this StepUp

		suspiciousIDX <- which(suspiciousPV[1:startIDX_SUD])
		
		# perhaps no pValue is suspicious, thus we do not reject anything
		if (length(suspiciousIDX) == 0)
			return(rep(FALSE, times = len))
		
		# There must be some pValue between 1 and startIDX that is suspicious
		# Searching the last one. Anything before (including the maximum) will be rejected.
		maxIDX <- max(suspiciousIDX)
		
		rejected[sortedPV$ix[1:maxIDX]] <- TRUE 
	}
	
	return(rejected)
}

#' A general step-down procedure.
#' 
#' Suppose we have n pValues and they are already sorted.
#' The procedure starts with comparing pValues[1] with criticalValues[1]. If 
#' pValues[1] <= criticalValues[1], then the hypothsis associated with pValues[1] is rejected and the algorithm 
#' carries on with second smallest pValue and criticalValue in the same way. The algorithm stops
#' rejecting at the first index i for which pValues[i] > criticalValues[i]. 
#' Thus pValues[j] is rejected if and only if pValues[i] <= criticalValues[i] for all i <= j.
#' @param pValues pValues to be used.
#' @param criticalValues criticalValues for the step-down procedure
#' @return rejected logical vector indicating if hypotheses are rejected or retained.
#' @author MarselScheer
#' @export
SD <- function(pValues, criticalValues) 
{
	SUD(criticalValues = criticalValues, 
			pValues = pValues,
			startIDX = 1)
}

#' A general step-up procedure.
#'
#' Suppose we have n pValues and they are already sorted. 
#' The procedure starts with comparing pValues[n] with criticalValues[n]. If
#' pValues[n] > criticalValues[n], then the hypothesis associated with pValues[n] 
#' is retained and the algorithm carries on with the next pValue and criticalValue, here for example pValues[n-1]
#' and criticalValues[n-1]. The algorithm stops retaining at the first index i for
#' which pValues[i] <= criticalValues[i]. Thus pValues[j] is rejected if and only if
#' their exists an index i with j <= i and pValues[i] <= criticalValues[i].
#' @param pValues pValues to be used.
#' @param criticalValues criticalValues for the step-up procedure
#' @return rejected logical vector indicating if hypotheses are rejected or retained.
#' @author MarselScheer
#' @export
SU <- function(pValues, criticalValues) 
{
	SUD(criticalValues = criticalValues, 
			pValues = pValues,
			startIDX = length(criticalValues))
}




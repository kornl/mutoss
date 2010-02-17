#' Applies a function to a Mutoss object.
#' 
#' This functions is intended for applying functions for multiplicity control on Mutoss class objects using the console and not the Mutoss GUI.
#' @param mutossObj the Mutoss object the function should be applied to
#' @param f the function that should be applied
#' @param label the label all slots of the Mutoss object should get that where changed
#' @param ... additional parameters that are passed to the function
#' @return A Mutoss object after applying the given function
#' 
#' \item{mutossObj}{Object of S4 class Mutoss}
#' @author Kornelius Rohmeyer
#' @export
#' @examples
#' newObjectBonf <- mutoss.apply(new(Class="Mutoss", pValues=runif(10)), f=bonferroni, label="Bonferroni Correction", alpha=0.05)
#' \dontrun{ TODO: EXAMPLE PROBLEM
#' newObjectHolm <- mutoss.apply(new(Class="Mutoss", pValues=runif(10)), f=holm, label="Holm's step-down-procedure", alpha=0.05, silent=T)
#' 
#' newObjectAORC <- mutoss.apply(new(Class="Mutoss", pValues=runif(10)), f=aorc, label="Asymtotically optimal rejection curve", alpha=0.05, startIDX_SUD = 1, silent=T)
#' }
mutoss.apply <- function(mutossObj, f, label, ...) {
	params <- list()	
	for (param in names(formals(f))) {					#runs over all parameters of f
		if (param %in% slotNames(mutossObj)) {			#checks if parameter name corresponds to a mutoss slot	
			paramTail <- list(slot(mutossObj, param))	#extracts values from appropriate slot
			names(paramTail) <- param 					#attaches names for parameter values
			params <- c(params, paramTail)				#concatenates parameters with and without data from objects
		}
	}	
	result <- eval(as.call(c(f,params,...)))			#evaluates function call with extracted parameters	
	for (param in names(result)) {						
		if (param %in% slotNames(mutossObj)) {
			value <- result[param][[1]]								
			attr(value, "method.name") <- label			#attaches attributes to 
			slot(mutossObj, param) <- value				#writes result in corresponding mutoss slots
		}
	}	
	return(mutossObj)
}




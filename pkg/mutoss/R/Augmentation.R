#' Wrapper function to the augmentation methods of the multtest package.
#' 
#' The augmentation method turns a vector of p-values which are already adjusted for FWER control
#' into p-values that are adjusted for gFWER, FDX or FDR. The underlying idea (for gFWER and FDX) 
#' is that the set of hypotheses rejected at a given level alpha under FWER can be "augmented"
#' by rejecting some additional hypotheses while still ensuring (strong) control of the desired weaker type I criterion.
#' For FDR, it uses the fact that FDX control for q=alpha=1-sqrt(1-beta) entails FDR control at level beta.
#' 
#' Use of these augmentation methods is recommended only in the situation where FWER-controlled p-values are
#' directly available from the data (using some specific method). When only marginal p-values are available,
#' it is generally prerefable to use other adjustment methods directly aimed at the intended criterion
#' (as opposed to first adjust for FWER, then augment)
#' 
#' Note: in the multtest package, two methods ("restricted" and "conservative") are available for FDR augmentation. 
#' Here the 'restricted' method is forced for FDR augmentation since it is in fact always valid and better than 
#' "conservative" (M. van der Laan, personal communication) with respect to power.
#' 
#' @param adjPValues         a vector of p-values that are *already* adjusted for FWER
#' @param newErrorControl    new error control type to adjust for. One of ("gFWER", "FDX", "FDR")
#' @param newK               k-parameter if newErrorControl is "gFWER"
#' @param newQ               q-parameter if newErrorControl is "FDX"
#' @param silent             if true any output on the console will be suppressed.  
#' @return A list containing:
#' 
#'	\item{adjPValues}{A numeric vector containing the new adjusted pValues}
#' 
#'	\item{rejected}{A logical vector indicating which hypotheses are rejected. Currently always NULL.}
#' 
#' 	\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function.}
#' 
#' @author KornRohm, GillesBlanchard
#' @references Dudoit, S. and van der Laan, M.J. (2008)
#'     Multiple Testing Procedures with Applications to Genomics, Springer. (chapter 6)
#' @examples
#' \dontrun{ TODO NH (MS) EXAMPLE PROBLEM
#' p <- c(runif(50), runif(50, 0, 0.01))
#' fwer_adj <- bonferroni(p)    # adjust for FWER using Bonferroni correction
#' fdx_adj  <- augmentation(fwer_adj$adjPValues , "FDX", q=0.1, silent = TRUE) #augment to FDX (q=0.1)
#' }
#' @export
augmentation <- function(adjPValues, newErrorControl, newK, newQ, silent=FALSE) {
	require(multtest)
	if (newErrorControl == "gFWER") {	
		out = fwer2gfwer(adjPValues, k = newK)
		if(!silent) cat('\n\n\t\tGeneralized Family-Wise Error Rate\n\n')
		return(list(adjPValues=as.numeric(out), rejected=NULL, errorControl = new(Class='ErrorControl',type="gFWER")))
	} else if (newErrorControl == "FDX") {	
		out = fwer2tppfp(adjPValues, q = newQ)
		if(!silent) cat('\n\n\t\tTail Probability of the Proportion of False Positives\n\n')
		return(list(adjPValues=as.numeric(out), rejected=NULL, errorControl = new(Class='ErrorControl',type="FDX")))
	} else if (newErrorControl == "FDR") {	
		out = fwer2fdr(adjPValues, method = "restricted")
		if(!silent) cat('\n\n\t\tFalse Discovery Rate\n\n')
		return(list(adjPValues=as.numeric(out$adjp), rejected=NULL, errorControl = new(Class='ErrorControl',type="FDR")))		
	} else{ 
		if(!silent)cat('\n\n\t\tUnknown newErrorControl method')
	}
}

#' @export
mutoss.augmentation <- function() { return(new(Class="MutossMethod",
					label="Augmentation MTP adjusted p-values",
					errorControl=c("FWER"),
					callFunction="augmentation",
					output=c("adjPValues", "rejected", "errorControl"),
					info="<h2>Augmentation MTP adjusted p-values</h2>
<p>Wrapper function to the augmentation methods of the multtest package.</p>

<p>The augmentation method turns a vector of p-values which are already adjusted for FWER control
into p-values that are adjusted for gFWER, FDX or FDR. The underlying idea (for gFWER and FDX) 
is that the set of hypotheses rejected at a given level alpha under FWER can be 'augmented'
by rejecting some additional hypotheses while still ensuring (strong) control of the desired weaker type I criterion.
For FDR, it uses the fact that FDX control for q=alpha=1-sqrt(1-beta) entails FDR control at level beta.</p>

<p>Use of these augmentation methods is recommended only in the situation where FWER-controlled p-values are
directly available from the data (using some specific method). When only marginal p-values are available,
it is generally prerefable to use other adjustment methods directly aimed at the intended criterion
(as opposed to first adjust for FWER, then augment)</p>

<p>Note: In the multtest package, two methods ('restricted' and 'conservative') are available for FDR augmentation. 
Here the 'restricted' method is forced for FDR augmentation since it is in fact always valid and better than 
'conservative' (M. van der Laan, personal communication) with respect to power.</p>
<h3>Reference:</h3>
<ul>
<li> S. Dudoit, M.J. van der Laan. \"<i> Multiple Testing Procedures with Applications to Genomics</i>\", Springer, 2008. (chapter 6) </li>\n\
<li></li>
</ul>",
# TODO: <- Add the possibility of filling the rejected slot? (need additional input alpha)
					parameters=list(adjPValues=list(type="numeric"),
								newErrorControl=list(type="character", label="New error control", choices=c("FDR","FDX","gFWER")),
								newK=list(type="numeric", optional=TRUE, label="For gFWER set k"),
                                newQ=list(type="numeric", optional=TRUE, label="For FDX set q")
						
                                    	      ))) }

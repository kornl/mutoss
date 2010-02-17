##################### multiple endpoints plus one covariable #############################################
###########################################################################

#' @export
#' @nord
multendponecov.model <- function(covariate, addcov) {
	covariate <- as.vector(covariate)
	if (missing(addcov)) {
		return(list(model=list(typ="multendponecov", covariate=covariate)))
	} else {
	addcov <- as.matrix(addcov)## eventuell hier ein cbind
	return(list(model=list(typ="multendponecov", covariate=covariate, addcov=addcov)))
}
}

#' @export
#' @nord
mutoss.multendponecov.model <- function() { return(new(Class="MutossMethod",
					label="Two sample test",
					callFunction="multendponecov.model",
					output=c("model"),
					info="<h2>Multiple Endpoints with one Covariate.</h2>
							<p>The input for this F test model is a data matrix whose columns represent the samples and the rows represent the multiple endpoints. 
							E.g. for genomics this would be a gene matrix, where each row gives the expression for a single gene.</p>
							<p> Furthermore, a covariate of any type is needed, e.g. age, tumour status, or another marker status.</p>
							<p> For the next steps you have the following choices:</p>
							<p> Either marginal hypotheses tests (if robust Kruskal-Wallis test, otherwise F-test) could be performed on each row of the 
							data matrix to obtain raw p-values which then need to be adjusted for multiplicity to control a chosen error rate.</p>
							<p> Or resampling based methods could be performed based on Dudoit and van der Laan (2007) to obtain adjusted p-values which control the 
							FWER. Afterwards it is possible to use augmentation procedures to get adjusted p-values for control of FDR, FDX or gFWER.</p> 
							<h3>Reference:</h3>
							<ul>
							<li>Dudoit, S. and van der Laan, M.J. (2007). <i>Mulitple Testing Procedures and Applications to Genomics.</i> Springer Series in Statistics.</li>
							</ul>",
					parameters=list(
							covariate=list(type="RObject", label="covariate of interest"),
							addcov=list(type="RObject", label="Additional covariates", optional=TRUE)
					)
			)) }


#' @export
#' @nord
mutoss.multendponecov.marginal.model <- function(model) {
	return("typ" %in% names(model) && model$typ == "multendponecov")
}

#' @export
#' @nord
multendponecov.marginal <- function(data, model, robust, alternative, psi0) {
	require(MASS)
	result <- NULL
	if (robust) {
		result <- apply(data, 1, function(x) {rlm(x$p.value} )
	} else {
		result <- apply(data, 1, function(x) {lm(x)$p.value} )
	}
	return(list(pValues=result))
}

#' @export
#' @nord
mutoss.mendponecov.marginal <- function() { return(new(Class="MutossMethod",
					label="Two sample test",
					callFunction="twosamp.marginal",
					output=c("pValues"),
					info="<h2></h2>
							<p>Robust = paired Wilcoxon. Otherwise paired t-test.</p> 
							<p></p>
							<h3>Reference:</h3>
							<ul>
							<li>todo.</li>
							</ul>",
					parameters=list(
							data=list(type="ANY"),
							model=list(type="ANY"),
							robust=list(type="logical", label="Robust statistic"),
							alternative=list(type="character", label="Alternative", choices=c("two.sided", "less", "greater")),
							psi0=list(type="numeric", label="Hypothesized null value", default=0),
							equalvar=list(type="logical", label="Equal variance")
					)
			)) }


#' @export
#' @nord
mutoss.multendponecov.multtest.model <- function(model) {
	return("typ" %in% names(model) && model$typ == "multendponecov")
}
#' @export
#' @nord
mendponecov.multtest <- function(data, model, alternative, robust, psi0, equalvar, alpha, nulldist, B=1000, method, seed=12345) {
	require(multtest)
	if (equalvar) {
	result <- MTP(X=data, W = NULL, Y = model$classlabel, Z = NULL, Z.incl = NULL, Z.test = NULL, 
			na.rm = TRUE, test = "t.twosamp.equalvar", robust = robust, 
			standardize = TRUE, alternative = alternative, psi0 = psi0, 
			typeone = "fwer", k = 0, q = 0.1, fdr.method = "restricted", 
			alpha = alpha, smooth.null = FALSE, nulldist = nulldist, 
			B = B, ic.quant.trans = FALSE, MVN.method = "mvrnorm", 
			penalty = 1e-06, method = method, get.cr = FALSE, get.cutoff = FALSE, 
			get.adjp = TRUE, keep.nulldist = FALSE, keep.rawdist = FALSE, 
			seed = seed, cluster = 1, type = NULL, dispatch = NULL, marg.null = NULL, 
			marg.par = NULL, keep.margpar = TRUE, ncp = NULL, perm.mat = NULL, 
			keep.index = FALSE, keep.label = FALSE)
			}
	else {
	result <- MTP(X=data, W = NULL, Y = model$classlabel, Z = NULL, Z.incl = NULL, Z.test = NULL, 
			na.rm = TRUE, test = "t.twosamp.unequalvar", robust = robust, 
			standardize = TRUE, alternative = alternative, psi0 = psi0, 
			typeone = "fwer", k = 0, q = 0.1, fdr.method = "restricted", 
			alpha = alpha, smooth.null = FALSE, nulldist = nulldist, 
			B = B, ic.quant.trans = FALSE, MVN.method = "mvrnorm", 
			penalty = 1e-06, method = method, get.cr = FALSE, get.cutoff = FALSE, 
			get.adjp = TRUE, keep.nulldist = FALSE, keep.rawdist = FALSE, 
			seed = seed, cluster = 1, type = NULL, dispatch = NULL, marg.null = NULL, 
			marg.par = NULL, keep.margpar = TRUE, ncp = NULL, perm.mat = NULL, 
			keep.index = FALSE, keep.label = FALSE)
	}
	return(list(adjPValues=result@adjp, rejected=as.vector(result@reject)))
}


#' @export
#' @nord
mutoss.mendponecov.multtest <- function() { return(new(Class="MutossMethod",
					label="Resampling-based regression of multiple endpoints and one covariate",
					errorControl="FWER",
					callFunction="mendponecov.multtest",
					output=c("adjPValues", "rejected"),
					info="<h2>Resampling-based regression of m</h2>
							<p>at present nulldist="ic" is incorporated. if you wish to use it use library(multtest).
							The default parameters for quantile transformation are used. </p> 
							<p>t-statistic for test of regression coefficients for the covariable in linear models, each row of the data matrix as outcome,
							possibly adjusted by other covariates (in the case of no covariates one recovers the one-sample t-statistics)</p>
							<h3>Reference:</h3>
							<ul>
							<li>todo</li>
							</ul>",
					parameters=list(
							data=list(type="ANY"),
							model=list(type="ANY"),
							robust=list(type="logical", label="Robust statistic"),
							alternative=list(type="character", label="Alternative", choices=c("two.sided", "less", "greater")),
							psi0=list(type="numeric", label="Hypothesized null value", default=0),
							equalvar=list(type="logical", label="Equal variance"),
							alpha=list(type="numeric"),
							nulldist=list(type="character", label="Resampling Method", choices=c("boot.cs", "boot.ctr", "boot.qt", "perm")),
							B=list(type="numeric", label="Number of Resampling Iterations", default=1000),
							method=list(type="character", label="Adjustment Method", choices=c("sd.minP","sd.maxT","ss.minP","ss.maxT"),
									seed=list(type="ANY", default=12345))
					)
			)) }


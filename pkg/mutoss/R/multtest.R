#######################################################################
#################################one sample test ######################

#' @export
#' @nord
onesamp.model <- function() {
	return(list(model=list(typ="onesamp")))
}

#' @export
#' @nord
mutoss.onesamp.model <- function() { return(new(Class="MutossMethod",
					label="One-sample test",
					callFunction="onesamp.model",
					output=c("model"),
					info="<h2>One sample model</h2>
							<p>The input for this one sample model is a data matrix whose columns represent the samples and the rows represent the multiple endpoints. 
							E.g. for genomics this would be a gene matrix, where each row gives the expression for a single gene.</p> 
							<p> For the next steps you have the following choices:</p>
							<p> Either marginal hypotheses tests (if robust Wilcoxon otherwise t-test) could be performed on each row of the 
							data matrix to obtain raw p-values which then need to be adjusted for multiplicity to control a chosen error rate.</p>
							<p> Or resampling based methods could be performed based on Dudoit and van der Laan (2007) to obtain adjusted p-values which control the 
							FWER. Afterwards it is possible to use augmentation procedures to get adjusted p-values for control of FDR, FDX or gFWER.</p> 
							<h3>Reference:</h3>
							<ul>
							<li>Dudoit, S. and van der Laan, M.J. (2007). <i>Mulitple Testing Procedures and Applications to Genomics.</i> Springer Series in Statistics.</li>
							</ul>",
					parameters=list(
					)
			)) }

#' @export
#' @nord
onesamp.marginal <- function(data, model, robust, alternative, psi0) {
	result <- NULL
	if (robust) {
		result <- apply(data, 1, function(x) {wilcox.test(x, alternative=alternative, mu=psi0)$p.value} )
	} else {
		result <- apply(data, 1, function(x) {t.test(x ,alternative=alternative, mu=psi0)$p.value} )
	}
	return(list(pValues=result))
}

#' @export
#' @nord
mutoss.onesamp.marginal.model <- function(model) {
	return("typ" %in% names(model) && model$typ == "onesamp")
}


#' @export
#' @nord
mutoss.onesamp.marginal <- function() { return(new(Class="MutossMethod",
					label="One-sample test",
					callFunction="onesamp.marginal",
					output=c("pValues"),
					info="<h2>Marginal one sample test.</h2>
							<p>The robust version uses the Wilcoxon-Mann-Whitney test, otherwise a t-test will be performed.</p> 
							<h3>Reference:</h3>
							<ul>
							<li>Wilcoxon, F. (1945). <i>Individual Comparisons by Ranking Methods.</i> Biometrics Bulletin 1:80-83.</li>\n\
							<li>Mann, H. and Whitney, D. (1947). <i>On a test of whether one of two random variables is stochastically larger 
							than the other.</i> Annals of Mathematical Statistics 18:50-60</li>\n\
							<li>Student (1908).<i>The probable error of a mean.</i> Biometrika, 6(1):1-25.</li>\n 
							</ul>",
					parameters=list(
							data=list(type="ANY"),
							model=list(type="ANY"),
							robust=list(type="logical", label="Robust statistic"),
							alternative=list(type="character", label="Alternative", choices=c("two.sided", "less", "greater")),
							psi0=list(type="numeric", label="Hypothesized null value", default=0)
					)
			)) }


#' @export
#' @nord
onesamp.multtest <- function(data, model, alternative, robust, psi0, alpha, nulldist, B=1000, method, seed=12345) {
	require(multtest)
	result <- MTP(X=data, W = NULL, Y = NULL, Z = NULL, Z.incl = NULL, Z.test = NULL, 
			na.rm = TRUE, test = "t.onesamp", robust = robust, 
			standardize = TRUE, alternative = alternative, psi0 = psi0, 
			typeone = "fwer", k = 0, q = 0.1, fdr.method = "restricted", 
			alpha = alpha, smooth.null = FALSE, nulldist = nulldist, 
			B = B, ic.quant.trans = FALSE, MVN.method = "mvrnorm", 
			penalty = 1e-06, method = method, get.cr = FALSE, get.cutoff = FALSE, 
			get.adjp = TRUE, keep.nulldist = FALSE, keep.rawdist = FALSE, 
			seed = seed, cluster = 1, type = NULL, dispatch = NULL, marg.null = NULL, 
			marg.par = NULL, keep.margpar = TRUE, ncp = NULL, perm.mat = NULL, 
			keep.index = FALSE, keep.label = FALSE)
	return(list(adjPValues=result@adjp, rejected=as.vector(result@reject)))
}

#' @export
#' @nord
mutoss.onesamp.multtest.model <- function(model) {
	return("typ" %in% names(model) && model$typ == "onesamp")
}

#' @export
#' @nord
mutoss.onesamp.multtest <- function() { return(new(Class="MutossMethod",
					label="Resampling-based one sample test",
					errorControl="FWER",
					callFunction="onesamp.multtest",
					output=c("adjPValues", "rejected"),
					info="<h2>Resampling-based one sample test</h2>
							<p>There are different choices of resampling methods available for estimating the joint test statistics null distribution:\n\
							<ul>
							<li>\"boot.cs\": non-parametric bootstrap with centering and scaling</li>\n\
							<li>\"boot.ctr\": centered-only bootstrap distribution</li>\n\
							<li>\"boot.qt\": quantile transformed bootstrap distribution. the default marginal t-distribution with n-1 degree of freedom is used.</li>\n\
							<li>\"perm\": permutation distribution (refering to the Westfall and Young procedures)</li>\n\
							<li>\"ic\": under GUI construction (available at (library(multtest)) </li>\n
							</ul>
							</p> 
							<p>There are four adjustment methods to control the FWER:
							<ul>
							<li>\"sd.minP\": step-down common-quantile procedure based on the minima of unadjusted p-values</li>
							<li>\"sd.maxT\": step-down common-cut-off procedure based on the maxima of test statistics</li>
							<li>\"ss.minP\": single-step common-quantile procedure</li>
							<li>\"ss.maxT\": single-step common-cut-off procedure</li>
							</ul>
							</p>
							<p> The default number of bootstrap iterations (or number of permutations if resampling method is \"perm\") is 1000. This can be reduced to increase the speed
							of computations, at a cost to precision. However, it is recommended to use a large number of resampling iterations, e.g. 10,000. </p>
							<p> The robust version uses the Wilcoxon-Mann-Whitney test, otherwise a t-test will be performed.</p>
							<h3>Reference:</h3>
							<ul>
							<li>Dudoit, S. and van der Laan, M.J. (2007). <i>Mulitple Testing Procedures and Applications to Genomics.</i> Springer Series in Statistics.</li>\n\
							<li>Westfall, P.H. and Young, S.S. (1993). <i>Resampling-Based Multiple Testing. Examples and Methods for p-value adjustment. </i>Wiley Series in Probability and Mathematical Statistics. </li>\n   
							</ul>",
					parameters=list(
							data=list(type="ANY"),
							model=list(type="ANY"),
							robust=list(type="logical", label="Robust statistic"),
							alternative=list(type="character", label="Alternative", choices=c("two.sided", "less", "greater")),
							psi0=list(type="numeric", label="Hypothesized null value", default=0),
							alpha=list(type="numeric"),
							nulldist=list(type="character", label="Resampling Method", choices=c("boot.cs", "boot.ctr", "boot.qt", "perm")),
							B=list(type="numeric", label="Number of Resampling Iterations", default=1000),
							method=list(type="character", label="Adjustment Method", choices=c("sd.minP","sd.maxT","ss.minP","ss.maxT"),
									seed=list(type="ANY", default=12345)
							)
					))) }


###########################################################################
###########################################################################

#' @export
#' @nord
paired.model <- function(classlabel) {
	classlabel <- as.vector(classlabel)
	return(list(model=list(typ="pairedsamp", classlabel=classlabel)))
}

#' @nord
#' @export
mutoss.paired.model <- function() { return(new(Class="MutossMethod",
					label="Paired sample test",
					callFunction="paired.model",
					output=c("model"),
					info="<h2>Paired sample test</h2>
							<p>The robust version uses the Wilcoxon test, otherwise a t-test will be performed.</p> 
							<p>The input for this paired sample model is a data matrix whose columns represent the samples and the rows represent the multiple endpoints. 
							E.g. for genomics this would be a gene matrix, where each row gives the expression for a single gene.</p> 
							<p> Furthermore, a classlabel needs to be provided to distinguish the two paired groups. The arrangement of group indices does not matter, as long
							as the columns are arranged in the same corresponding order between groups. For example, if group 1 is code as 0 and group 2 is 
							coded as 1, for 3 pairs of data, it does not matter if the classlabel is coded as (0,0,0,1,1,1) or (1,1,1,0,0,0) or (0,1,0,1,0,1)
							or (1,0,1,0,1,0), the paired differences between groups will be calculated as group2 - group1. 
							</p>
							<p> For the next steps you have the following choices:</p>
							<p> Either marginal hypotheses tests (if robust Wilcoxon otherwise t-test) could be performed on each row of the 
							data matrix to obtain raw p-values which then need to be adjusted for multiplicity to control a chosen error rate.</p>
							<p> Or resampling based methods could be performed based on Dudoit and van der Laan (2007) to obtain adjusted p-values which control the 
							FWER. Afterwards it is possible to use augmentation procedures to get adjusted p-values for control of FDR, FDX or gFWER.</p> 
							
							
							<h3>Reference:</h3>
							<ul>
							<li>Dudoit, S. and van der Laan, M.J. (2007). <i>Mulitple Testing Procedures and Applications to Genomics.</i> Springer Series in Statistics.</li>
							</ul>",
					parameters=list(
							classlabel=list(type="RObject", label="classlabel")
					)
			)) }

#' @export
#' @nord
paired.marginal <- function(data, model, robust, alternative, psi0) {
	label <- as.numeric(as.factor(model$classlabel))
	result <- NULL
	if (robust) {
		result <- apply(data, 1, function(x) {wilcox.test(x=x[label==1], y=x[label==2], alternative=alternative, mu=psi0, paired=TRUE)$p.value} )
	} else {
		result <- apply(data, 1, function(x) {t.test(x=x[label==1], y=x[label==2], alternative=alternative, mu=psi0, paired=TRUE)$p.value} )
	}
	return(list(pValues=result))
}

#' @export
#' @nord
mutoss.paired.marginal <- function() { return(new(Class="MutossMethod",
					label="Paired sample test",
					callFunction="paired.marginal",
					output=c("pValues"),
					info="<h2></h2>
							<p>The robust version uses the Wilcoxon test, otherwise a paired t-test will be performed.</p> 
							<p>A vector of classlabels needs to be provided to distinguish the two paired groups. The arrangement of group indices does not matter, as long
							as the columns are arranged in the same corresponding order between groups. For example, if group 1 is code as 0 and group 2 is 
							coded as 1, for 3 pairs of data, it does not matter if the classlabel is coded as (0,0,0,1,1,1) or (1,1,1,0,0,0) or (0,1,0,1,0,1)
							or (1,0,1,0,1,0), the paired differences between groups will be calculated as group2 - group1. </p>
							<p>You could either choose a valid R object to load as classlabels or you could provide it manually by inserting e.g. c(0,1,0,1,0,1) or rep(c(0,1), each=5) or rep(c(0,1), 5). </p>",
					parameters=list(
							data=list(type="ANY"),
							model=list(type="ANY"),
							robust=list(type="logical", label="Robust statistic"),
							alternative=list(type="character", label="Alternative", choices=c("two.sided", "less", "greater")),
							psi0=list(type="numeric", label="Hypothesized null value", default=0)
					)
			)) }

#' @export
#' @nord
mutoss.paired.marginal.model <- function(model) {
	return("typ" %in% names(model) && model$typ == "pairedsamp")
}

#' @export
#' @nord
paired.multtest <- function(data, model, alternative, robust, psi0, alpha, nulldist, B=1000, method, seed=12345) {
	require(multtest)
	result <- MTP(X=data, W = NULL, Y = model$classlabel, Z = NULL, Z.incl = NULL, Z.test = NULL, 
			na.rm = TRUE, test = "t.pair", robust = robust, 
			standardize = TRUE, alternative = alternative, psi0 = psi0, 
			typeone = "fwer", k = 0, q = 0.1, fdr.method = "restricted", 
			alpha = alpha, smooth.null = FALSE, nulldist = nulldist, 
			B = B, ic.quant.trans = FALSE, MVN.method = "mvrnorm", 
			penalty = 1e-06, method = method, get.cr = FALSE, get.cutoff = FALSE, 
			get.adjp = TRUE, keep.nulldist = FALSE, keep.rawdist = FALSE, 
			seed = seed, cluster = 1, type = NULL, dispatch = NULL, marg.null = NULL, 
			marg.par = NULL, keep.margpar = TRUE, ncp = NULL, perm.mat = NULL, 
			keep.index = FALSE, keep.label = FALSE)
	return(list(adjPValues=result@adjp, rejected=as.vector(result@reject)))
}

#' @export
#' @nord
mutoss.paired.multtest.model <- function(model) {
	return("typ" %in% names(model) && model$typ == "pairedsamp")
}

#' @export
#' @nord
mutoss.paired.multtest <- function() { return(new(Class="MutossMethod",
					label="Resampling-based paired sample test",
					errorControl="FWER",
					callFunction="paired.multtest",
					output=c("adjPValues", "rejected"),
					info="<h2>Resampling-based paired test</h2>
							<p>There are different choices of resampling methods available for estimating the joint test statistics null distribution:\n\
							<ul>
							<li>\"boot.cs\": non-parametric bootstrap with centering and scaling</li>\n\
							<li>\"boot.ctr\": centered-only bootstrap distribution</li>\n\
							<li>\"boot.qt\": quantile transformed bootstrap distribution. the default marginal t-distribution with n-1 degree 
							of freedom is used, where n is the number of pairs.</li>\n\
							<li>\"perm\": permutation distribution (refering to the Westfall and Young procedures)</li>\n\
							<li>\"ic\": under GUI construction (available at (library(multtest)) </li>\n
							</ul>
							</p> 
							<p>There are four adjustment methods to control the FWER:
							<ul>
							<li>\"sd.minP\": step-down common-quantile procedure based on the minima of unadjusted p-values</li>
							<li>\"sd.maxT\": step-down common-cut-off procedure based on the maxima of test statistics</li>
							<li>\"ss.minP\": single-step common-quantile procedure</li>
							<li>\"ss.maxT\": single-step common-cut-off procedure</li>
							</ul>
							</p>
							<p> The default number of bootstrap iterations (or number of permutations if resampling method is \"perm\") is 1000. This can be reduced to increase the speed
							of computations, at a cost to precision. However, it is recommended to use a large number of resampling iterations, e.g. 10,000. </p>
							<p> The robust version uses the Wilcoxon-Mann-Whitney test, otherwise a t-test will be performed.</p>
							<h3>Reference:</h3>
							<ul>
							<li>Dudoit, S. and van der Laan, M.J. (2007). <i>Mulitple Testing Procedures and Applications to Genomics.</i> Springer Series in Statistics.</li>\n\
							<li>Westfall, P.H. and Young, S.S. (1993). <i>Resampling-Based Multiple Testing. Examples and Methods for p-value adjustment. </i>Wiley Series in Probability and Mathematical Statistics. </li>\n   
							</ul>",
					parameters=list(
							data=list(type="ANY"),
							model=list(type="ANY"),
							robust=list(type="logical", label="Robust statistic"),
							alternative=list(type="character", label="Alternative", choices=c("two.sided", "less", "greater")),
							psi0=list(type="numeric", label="Hypothesized null value", default=0),
							alpha=list(type="numeric"),
							nulldist=list(type="character", label="Resampling Method", choices=c("boot.cs", "boot.ctr", "boot.qt", "perm")),
							B=list(type="numeric", label="Number of Resampling Iterations", default=1000),
							method=list(type="character", label="Adjustment Method", choices=c("sd.minP","sd.maxT","ss.minP","ss.maxT"),
									seed=list(type="ANY", default=12345))
					)
			)) }

#####################two sample test #############################################
###########################################################################

#' @export
#' @nord
twosamp.model <- function(classlabel) {
	classlabel <- as.vector(classlabel)
	return(list(model=list(typ="twosamp", classlabel=classlabel)))
}
# TODO: correct "pairedsamp" above?

#' @export
#' @nord
mutoss.twosamp.model <- function() { return(new(Class="MutossMethod",
					label="Two sample test",
					callFunction="twosamp.model",
					output=c("model"),
					info="<h2>Two sample model</h2>
							<p>The input for this one sample model is a data matrix whose columns represent the samples and the rows represent the multiple endpoints. 
							E.g. for genomics this would be a gene matrix, where each row gives the expression for a single gene.</p> 
							<p> Furthermore, a classlabel needs to be provided to distinguish the two sample groups.
							<p> For the next steps you have the following choices:</p>
							<p> Either marginal hypotheses tests (if robust Wilcoxon otherwise t-test) could be performed on each row of the 
							data matrix to obtain raw p-values which then need to be adjusted for multiplicity to control a chosen error rate.</p>
							<p> Or resampling based methods could be performed based on Dudoit and van der Laan (2007) to obtain adjusted p-values which control the 
							FWER. Afterwards it is possible to use augmentation procedures to get adjusted p-values for control of FDR, FDX or gFWER.</p> 
							<h3>Reference:</h3>
							<ul>
							<li>Dudoit, S. and van der Laan, M.J. (2007). <i>Mulitple Testing Procedures and Applications to Genomics.</i> Springer Series in Statistics.</li>
							</ul>",
					parameters=list(
							classlabel=list(type="RObject", label="classlabel")
					)
			)) }

#' @export
#' @nord
twosamp.marginal <- function(data, model, robust, alternative, psi0, equalvar) {
	label <- as.numeric(as.factor(model$classlabel))
	result <- NULL
	if (robust) {
		result <- apply(data, 1, function(x) {wilcox.test(x=x[ ,label==1], y=x[label==2], alternative=alternative, mu=psi0)$p.value} )
	} else {
		result <- apply(data, 1, function(x) {t.test(x=x[ ,label==1], y=x[label==2], alternative=alternative, mu=psi0, equal.var=equalval)$p.value} )
	}
	return(list(pValues=result))
}

#' @export
#' @nord
mutoss.twosamp.marginal.model <- function(model) {
	return("typ" %in% names(model) && model$typ == "twosamp")
}

#' @export
#' @nord
mutoss.twosamp.marginal <- function() { return(new(Class="MutossMethod",
					label="Two sample test",
					callFunction="twosamp.marginal",
					output=c("pValues"),
					info="<h2></h2>
							<p>The robust version uses the Wilcoxon-Mann-Whitney test, otherwise a two-sample t-test will be performed.</p> 
							<h3>Reference:</h3>
							<ul>
							<li>Wilcoxon, F. (1945). <i>Individual Comparisons by Ranking Methods.</i> Biometrics Bulletin 1:80-83.</li>\n\
							<li>Mann, H. and Whitney, D. (1947). <i>On a test of whether one of two random variables is stochastically larger 
							than the other.</i> Annals of Mathematical Statistics 18:50-60</li>\n
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
twosamp.multtest <- function(data, model, alternative, robust, psi0, equalvar, alpha, nulldist, B=1000, method, seed=12345) {
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
mutoss.twosamp.multtest.model <- function(model) {
	return("typ" %in% names(model) && model$typ == "twosamp")
}

#' @export
#' @nord
mutoss.twosamp.multtest <- function() { return(new(Class="MutossMethod",
					label="Resampling-based two sample test",
					errorControl="FWER",
					callFunction="twosamp.multtest",
					output=c("adjPValues", "rejected"),
					info="<h2>Resampling-based two sample test</h2>
							<p>There are different choices of resampling methods available for estimating the joint test statistics null distribution:\n\
							<ul>
							<li>\"boot.cs\": non-parametric bootstrap with centering and scaling</li>\n\
							<li>\"boot.ctr\": centered-only bootstrap distribution</li>\n\
							<li>\"boot.qt\": quantile transformed bootstrap distribution. the default marginal t-distribution with n-1 degree of freedom is used.</li>\n\
							<li>\"perm\": permutation distribution (refering to the Westfall and Young procedures)</li>\n\
							<li>\"ic\": under GUI construction (available at (library(multtest)) </li>\n
							</ul>
							</p> 
							<p>There are four adjustment methods to control the FWER:
							<ul>
							<li>\"sd.minP\": step-down common-quantile procedure based on the minima of unadjusted p-values</li>
							<li>\"sd.maxT\": step-down common-cut-off procedure based on the maxima of test statistics</li>
							<li>\"ss.minP\": single-step common-quantile procedure</li>
							<li>\"ss.maxT\": single-step common-cut-off procedure</li>
							</ul>
							</p>
							<p> The default number of bootstrap iterations (or number of permutations if resampling method is \"perm\") is 1000. This can be reduced to increase the speed
							of computations, at a cost to precision. However, it is recommended to use a large number of resampling iterations, e.g. 10,000. </p>
							<p> The robust version uses the Wilcoxon-Mann-Whitney test, otherwise a t-test will be performed.</p>
							<h3>Reference:</h3>
							<ul>
							<li>Dudoit, S. and van der Laan, M.J. (2007). <i>Mulitple Testing Procedures and Applications to Genomics.</i> Springer Series in Statistics.</li>\n\
							<li>Westfall, P.H. and Young, S.S. (1993). <i>Resampling-Based Multiple Testing. Examples and Methods for p-value adjustment. </i>Wiley Series in Probability and Mathematical Statistics. </li>\n   
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



##################### F test #############################################
###########################################################################

#' @export
ftest.model <- function(classlabel) {
	classlabel <- as.vector(classlabel)
	return(list(model=list(typ="ftest", classlabel=classlabel)))
}

#' @export
mutoss.ftest.model <- function() { return(new(Class="MutossMethod",
					label="F test",
					callFunction="ftest.model",
					output=c("model"),
					info="<h2>F test</h2>
							<p>The input for this F test model is a data matrix whose columns represent the samples and the rows represent the multiple endpoints. 
							E.g. for genomics this would be a gene matrix, where each row gives the expression for a single gene.</p> 
							<p> Furthermore, a classlabel needs to be provided to distinguish k sample groups.</p>
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
							classlabel=list(type="RObject", label="classlabel")
					)
			)) }

#' @export
ftest.marginal <- function(data, model, robust) {
	label <- as.numeric(as.factor(model$classlabel))
	result <- NULL
	if (robust) {
		result <- apply(data, 1, function(x) {kruskal.test(x=x, g=label)$p.value} )
	} else {
		result <- apply(data, 1, function(x) { out=x
						anova(lm( out ~ label ))$'Pr(>F)'[1]} )	
	}	
	return(list(pValues=result))
}

#' @export
mutoss.ftest.marginal <- function() { return(new(Class="MutossMethod",
					label="F test",
					callFunction="ftest.marginal",
					output=c("pValues"),
					info="<h2></h2>
							<p>Robust = Kruskal-Wallis test. Otherwise F-test.</p> 
							<p></p>
							<h3>Reference:</h3>
							<ul>
							<li>Kruskal, W.H. und Wallis, W.A. (1952). <i>Use of ranks in one-criterion variance analysis.</i> JASA, 47:583-621</li>
							</ul>",
					parameters=list(
							data=list(type="ANY"),
							model=list(type="ANY"),
							robust=list(type="logical", label="Robust statistic")
					)
			)) }

#' @export
#' @nord
mutoss.ftest.marginal.model <- function(model) {
	return("typ" %in% names(model) && model$typ == "ftest")
}


#' @export
ftest.multtest <- function(data, model, robust, alpha, nulldist, B=1000, method, seed=12345) {
	require(multtest)
	result <- MTP(X=data, W = NULL, Y = model$classlabel, Z = NULL, Z.incl = NULL, Z.test = NULL, 
			na.rm = TRUE, test = "f", robust = robust, 
			standardize = TRUE, typeone = "fwer", 
			alpha = alpha, smooth.null = FALSE, nulldist = nulldist, 
			B = B, ic.quant.trans = FALSE, MVN.method = "mvrnorm", 
			penalty = 1e-06, method = method, get.cr = FALSE, get.cutoff = FALSE, 
			get.adjp = TRUE, keep.nulldist = FALSE, keep.rawdist = FALSE, 
			seed = seed, cluster = 1, type = NULL, dispatch = NULL, marg.null = NULL, 
			marg.par = NULL, keep.margpar = TRUE, ncp = NULL, perm.mat = NULL, 
			keep.index = FALSE, keep.label = FALSE)
	return(list(adjPValues=result@adjp, rejected=as.vector(result@reject)))
}


#' @export
mutoss.ftest.multtest <- function() { return(new(Class="MutossMethod",
					label="Resampling-based F test",
					errorControl="FWER",
					callFunction="ftest.multtest",
					output=c("adjPValues", "rejected"),
					info="<h2>Resampling-based F test</h2>
							<p>There are different choices of resampling methods available for estimating the joint test statistics null distribution:\n\
							<ul>
							<li>\"boot.cs\": non-parametric bootstrap with centering and scaling</li>\n\
							<li>\"boot.ctr\": centered-only bootstrap distribution</li>\n\
							<li>\"boot.qt\": quantile transformed bootstrap distribution. the default marginal F-distribution with df1=k-1, df2=n-k for k gruops is used.</li>\n\
							<li>\"perm\": permutation distribution (refering to the Westfall and Young procedures)</li>\n\
							<li>\"ic\": under GUI construction (available at (library(multtest)) </li>\n
							</ul>
							</p> 
							<p>There are four adjustment methods to control the FWER:
							<ul>
							<li>\"sd.minP\": step-down common-quantile procedure based on the minima of unadjusted p-values</li>
							<li>\"sd.maxT\": step-down common-cut-off procedure based on the maxima of test statistics</li>
							<li>\"ss.minP\": single-step common-quantile procedure</li>
							<li>\"ss.maxT\": single-step common-cut-off procedure</li>
							</ul>
							</p>
							<p> The default number of bootstrap iterations (or number of permutations if resampling method is \"perm\") is 1000. This can be reduced to increase the speed
							of computations, at a cost to precision. However, it is recommended to use a large number of resampling iterations, e.g. 10,000. </p>
							<p> The robust version uses the Kruskal-Wallis test, otherwise a F test will be performed.</p>
							
							<h3>Reference:</h3>
							<ul>
							<li>Dudoit, S. and van der Laan, M.J. (2007). <i>Mulitple Testing Procedures and Applications to Genomics.</i> Springer Series in Statistics.</li>\n\
							<li>Westfall, P.H. and Young, S.S. (1993). <i>Resampling-Based Multiple Testing. Examples and Methods for p-value adjustment. </i>Wiley Series in Probability and Mathematical Statistics. </li>\n   
							</ul>",
					parameters=list(
							data=list(type="ANY"),
							model=list(type="ANY"),
							robust=list(type="logical", label="Robust statistic"),
							alpha=list(type="numeric"),
							nulldist=list(type="character", label="Resampling Method", choices=c("boot.cs", "boot.ctr", "boot.qt", "perm")),
							B=list(type="numeric", label="Number of Resampling Iterations", default=1000),
							method=list(type="character", label="Adjustment Method", choices=c("sd.minP","sd.maxT","ss.minP","ss.maxT"),
									seed=list(type="ANY", default=12345)
							)))
	)}

#' @export
#' @nord
mutoss.ftest.multtest.model <- function(model) {
	return("typ" %in% names(model) && model$typ == "ftest")
}
\name{onesamp.multtest}
\alias{onesamp.multtest}
\title{Resampling-based one sample test}
\usage{onesamp.multtest(data, alternative, robust, psi0, alpha,
    nulldist, B=1000, method, seed=12345)}
\description{Pertforms a resampling-based one sample test}
\references{
 Dudoit, S. and van der Laan, M.J. (2007). Mulitple Testing Procedures and Applications to Genomics.Springer Series in Statistics.
 Westfall, P.H. and Young, S.S. (1993). Resampling-Based Multiple Testing. Examples and Methods for p-value adjustment. Wiley Series in Probability and Mathematical Statistics.   
}
\arguments{
  \item{data}{the data set}
  \item{alternative}{a character string specifying the alternative hypothesis,
          must be one of \code{two.sided}, \code{greater} or
          \code{less}}
  \item{robust}{a logical variable indicating whether a t-test or a Wilcoxon-Mann-Whitney test should be used.}
  \item{psi0}{a numeric that defines the hypothesized null value}
  \item{alpha}{}
  \item{nulldist}{There are different choices of resampling methods available for estimating the joint test statistics null distribution:
	\itemize{
		\item{ boot.cs: non-parametric bootstrap with centering and scaling}
		\item{ boot.ctr: centered-only bootstrap distribution}
		\item{ boot.qt: quantile transformed bootstrap distribution. the default marginal t-distribution with n-1 degree of freedom is used.}
		\item{ perm: permutation distribution (refering to the Westfall and Young procedures)}
		\item{ ic: under GUI construction (available at (library(multtest))}
	}}
  \item{B}{}
  \item{method}{There are four adjustment methods to control the FWER:
	\itemize{
		\item{ sd.minP: step-down common-quantile procedure based on the minima of unadjusted p-values}
		\item{ sd.maxT: step-down common-cut-off procedure based on the maxima of test statistics}
		\item{ ss.minP: single-step common-quantile procedure}
		\item{ ss.maxT: single-step common-cut-off procedure}
	}}
  \item{seed}{}
}
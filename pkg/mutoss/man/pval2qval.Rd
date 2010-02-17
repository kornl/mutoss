\name{pval2qval}
\alias{pval2qval}
\title{Strimmer et al.'s fdrtool-based q-values}
\usage{pval2qval(pValues, cutoff)}
\description{The function \code{pval2qval} takes a vector of p-values and
estimates for each case the tail area-based FDR, which can be regarded as a p-value corrected for multiplicity. 
This is done by calling the \code{fdrtool} function.
If a cutoff is supplied, a vector of rejected hypotheses will be returned as well.}
\value{A list containing:
\item{qValues}{A numeric vector with one q-value for each hypothesis.}
\item{rejected}{A logical vector indicating rejection/retention for each hypothesis when \code{cutoff} is supplied.}}
\author{JonathanRosenblatt}
\references{Strimmer, K. (2008). fdrtool: a versatile R package for estimating local and tail
area-based false discovery rates. Bioinformatics 24: 1461-1462. \cr
Storey, J. D. (2003)  The Positive False Discovery Rate: A Bayesian Interpretation and the q-Value.
The Annals of Statistics 31(6): 2013-2035}
\arguments{\item{pValues}{Numeric vector of p-values to be used.}
\item{cutoff}{The positive FDR cutoff for rejection. Hypotheses with \code{qValues} smaller then \code{cutoff} will be rejected.}}
\examples{pvals<- runif(1000)^2
pval2qval(pvals)
pval2qval(pValues=pvals, cutoff=0.1)}

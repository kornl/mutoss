\name{ranktruncated}
\alias{ranktruncated}
\title{Rank truncated p-Value procedure...}
\usage{ranktruncated(pValues, K, silent=FALSE)}
\description{Rank truncated p-Value procedure
The program computes the exact distribution and with it the p-Value}
\details{This function computes the exact distribution of the product of
at most K significant p-values of \eqn{L>K} observed p-values. Thus, one gets the
pvalue from the exact distribution. This has certain advantages for genomewide 
association scans: K can be chosen on the basis of a hypothesised 
disease model, and is independent of sample size. Furthermore, 
the alternative hypothesis corresponds more closely to the 
experimental situation where all loci have fixed effects.}
\value{Used.pValue: List information about the used pValues; RTP: Test statistic and pValue}
\author{Frank Konietschke}
\references{Dubridge, F., Koeleman, B.P.C. (2003). Rank truncated product of P-values, with application to genomewide association scans. Genet Epidemiol. 2003 Dec;25(4):360-6}
\arguments{\item{pValues}{Vector of p-Values (not sorted)}
\item{K}{the number of hypotheses / p-Values being in w}
\item{silent}{If true any output on the console will be suppressed.}}
\examples{pvalues<-runif(10)
result <- ranktruncated(pvalues,K=2,silent=FALSE) # take the K=2 smallest pvalues
result <- ranktruncated(pvalues,K=2,silent=TRUE) # take the K=2 smallest pvalues
result <- ranktruncated(pvalues,K=5,silent=TRUE) # take the K=5 smallest pvalues}

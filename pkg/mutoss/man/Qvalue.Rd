\name{Qvalue}
\alias{Qvalue}
\title{Storey's (2001) q-value Procedure...}
\usage{Qvalue(pValues, lambda=seq(0, 0.9, 0.05), pi0.method="smoother",
    fdr.level=NULL, robust=FALSE, smooth.df=3, smooth.log.pi0=FALSE,
    silent=FALSE)}
\description{Storey's (2001) q-value Procedure}
\details{The Qvalue procedure estimates the q-values for a given set of p-values. The q-value of a test measures the
proportion of false positive incurred when that particular test is called sigificant.
It gives the scientist a hypothesis testing error measure for each observed statistic with respect to the pFDR.

Note: If no options are selected, then the method used to estimate pi0 is the smoother method desribed in Storey and Tibshirani (2003). 
The bootstrap method is described in Storey, Taylor and Siegmund (2004).}
\value{A list containing:

\item{qValues}{A vector of the estimated q-values}

\item{pi0}{An estimate of the proportion of null hypotheses} 

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function.}}
\author{HackNiklas}
\references{Storey, John (2001). The Positive False Discovery Rate: A Baysian Interpretation and the Q-Value.
The Annals of Statistics, Vol. 31, No. 6, 2013-2035.}
\arguments{\item{pValues}{pValues to be used (only necessary input)}
\item{lambda}{Value of the tuning parameter to be used}
\item{pi0.method}{Method for automatically choosing tuning parameter in the estimation of pi_0. Either 'smoother' or 'bootstrap'}
\item{fdr.level}{Level at which to control the FDR}
\item{robust}{Logical, whether to make estimate more robust for small p-values.}
\item{smooth.df}{Number of degrees of freedom to use when estimating pi_0 with the smoother.}
\item{smooth.log.pi0}{Logical, if TRUE and pi0.method = 'smoother', pi0 will be estimated by applying a smoother 
to a scatterplot of log(pi_0) estimates against the tuning parameter lambda.}
\item{silent}{logical scalar. If \code{TRUE} no output is generated.}}
\examples{pval <- c(runif(50), runif(50, 0, 0.01))
result <- Qvalue(pval)
result <- Qvalue(pval, lambda=0.5)}

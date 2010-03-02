\name{two.stage}
\alias{two.stage}
\title{A p-value procedure which controls the FDR for independent test statistics.}
\usage{two.stage(pValues, alpha)}
\description{A p-value procedure which controls the FDR for independent test statistics.}
\details{In the Benjamini-Krieger-Yekutieli two-stage procedure the linear step-up procedure is used in
stage one to estimate m0 which is re-plugged in a linear step-up. 
This procedure is more powerful then non-adaptive procedures, while still controlling the FDR. 
On the other hand, error control is not guaranteed under dependence in which case more conservative procedures should be used (e.g. BH).}
\value{A list containing:
\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test.} 

\item{adjPValues}{A numeric vector containing adjusted p-values.}

\item{pi0}{An estimate of the proportion of true null hypotheses among all hypotheses (pi0=m0/m). }

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}}
\author{JonathanRosenblatt}
\arguments{\item{pValues}{A numeric vecor of p-values.}
\item{alpha}{The FDR error rate to control.}}
\examples{pvals<- runif(100)^2
two.stage(pvals, 0.1)}

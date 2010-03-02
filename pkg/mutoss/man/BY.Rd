\name{BY}
\alias{BY}
\title{Benjamini-Yekutieli (2001) step-up procedure}
\usage{BY(pValues, alpha, silent=FALSE)}
\description{The Benjamini-Yekutieli step-up procedure is applied to pValues. 
The procedure ensures FDR control for any dependency structure.}
\details{The critical values of the Benjamini-Yekutieli (BY) procedure are calculated by 
replacing the alpha of the Benjamini-Hochberg procedure by alpha/sum(1/1:m)), i.e.,
c(i)=i*alpha/(m*(sum(1/1:m))) for i=1,...,m. For large number m of hypotheses the critical values of the BY procedure and the 
BH procedure differ by a factor log(m). Benjamini and Yekutieli (2001) showed that this step-up procedure controls
the FDR at level alpha*m/m0 for any dependency structure among the test statistics.}
\value{A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test} 

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}}
\author{WerftWiebke}
\references{Benjamini, Y. and Yekutieli, D. (2001). The control of the false discovery rate in multiple testing under dependency.
Annals of Statistics, 29(4):1165-1188.}
\arguments{\item{pValues}{The used unadjusted pValues.}
\item{alpha}{The level at which the FDR shall be controlled.}
\item{silent}{If true any output on the console will be suppressed.}}
\examples{alpha <- 0.05
p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9, max=1))
result <- BY(p, alpha)
result <- BY(p, alpha, silent=TRUE)}

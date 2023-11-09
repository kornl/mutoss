\name{adaptiveBH}
\alias{adaptiveBH}
\title{Benjamini-Hochberg (2000) adaptive linear step-up procedure}
\usage{adaptiveBH(pValues, alpha, silent=FALSE)}
\description{The adaptive Benjamini-Hochberg step-up procedure is applied to pValues.
It controls the FDR at level alpha for independent or positive regression dependent test statistics.}
\details{In the adaptive Benjamini-Hochberg step-up procedure the number of true null hypotheses is estimated first as in Hochberg and
Benjamini (1990), and this estimate is used in the procedure of Benjamini and
Hochberg (1995) with alpha'=alpha*m/m0.
Please note that this method is not equivalent to multcomp's ABH. They revised the formular of the original paper.
}
\value{A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test} 

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{pi0}{An estimate of the proportion of true null hypotheses among all hypotheses (pi0=m0/m). }

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}}
\author{WerftWiebke}
\references{Benjamini, Y. and Hochberg, Y. (2000). On the Adaptive Control of the False Discovery Rate
in Multiple Testing With Independent Statistics.
Journal of Educational and Behavioral Statistics, 25(1): 60-83.\eqn{n}

Hochberg, Y. and Benjamini, Y. (1990). More powerful procedures for multiple significance testing. 
Statistics in Medicine 9, 811-818.\eqn{n}

Benjamini, Y. and Hochberg, Y. (1995). Controlling the false discovery rate: A practical and powerful approach to mulitple testing.
Journal of the Royal Statistical Society, Series B, 57:289-300.}
\arguments{\item{pValues}{The used raw pValues.}
\item{alpha}{The level at which the FDR shall be controlled.}
\item{silent}{If true any output on the console will be suppressed.}}
\examples{alpha <- 0.05
p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9, max=1))
result <- adaptiveBH(p, alpha)
result <- adaptiveBH(p, alpha, silent=TRUE)}

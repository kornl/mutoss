\name{SidakSD}
\alias{SidakSD}
\title{Sidak-like (1987) step-down procedure}
\usage{SidakSD(pValues, alpha, silent=FALSE)}
\description{The Sidak-like (1987) step-down procedure is applied to pValues
The Sidak-like step-down procedure is an improvement over the Holm's (1979) 
step-down procedure. The improvement is analogous to Sidak's correction 
over the original Bonferroni procedure. This Sidak-like step-down procedure 
assumes positive orthant dependent test statistics.}
\value{A list containing:

\item{adjPValues}{a numeric vector containing the adjusted pValues}

\item{rejected}{a logical vector indicating which hypotheses are rejected}

\item{criticalValues}{a numeric vector containing critical values used in the step-up-down test}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}}
\author{WerftWiebke}
\references{Hollander, B.S. and Covenhaver, M.D. (1987). An Improved Sequentially Rejective Bonferroni Test Procedure.
Biometrics, 43(2):417-423, 1987.}
\arguments{\item{pValues}{The used raw pValues.}
\item{alpha}{The level at which the FWER shall be controlled.}
\item{silent}{If true any output on the console will be suppressed.}}
\examples{alpha <- 0.05
p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9,max=1))
result <- SidakSD(p, alpha)
result <- SidakSD(p, alpha, silent=TRUE)}

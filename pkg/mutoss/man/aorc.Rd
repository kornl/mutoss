\name{aorc}
\alias{aorc}
\title{Step-up-down test based on the asymptotically optimal rejection curve.}
\usage{aorc(pValues, alpha, startIDX_SUD=length(pValues), betaAdjustment,
    silent=FALSE)}
\description{Performs a step-up-down test on pValues. The critical values are based 
on the asymptotically optimal rejection curve. To have finite FDR control,
an automatic adjustment of the critical values is done (see
details for more).}
\details{The graph of the function f(t) = t / (t * (1 - alpha) + alpha) is called the asymptotically
optimal rejection curve. Denote by finv(t) the inverse of f(t). Using the 
critical values finv(i/n) for i = 1, ..., n yields asymptotic FDR control.
To ensure finite control it is possible to adjust f(t) by a factor. The
function calculateBetaAdjustment() calculates a beta such that (1 + beta / n) * f(t)
can be used to control the FDR for a given finite sample size. If the 
parameter betaAdjustment is not provided, calculateBetaAdjustment() will be called automatically.}
\value{A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error (FDR)
controlled by the function and the level \code{alpha}.}}
\references{Finner, H., Dickhaus, T. & Roters, M. (2009).
On the false discovery rate and an asymptotically optimal rejection curve.
The Annals of Statistics 37, 596-618.}
\author{MarselScheer}
\arguments{\item{pValues}{pValues to be used. They are assumed to be stochastically independent.}
\item{alpha}{The level at which the FDR shall be controlled.}
\item{startIDX_SUD}{The index at which critical value the 
step-up-down test starts. Default is length(pValues) and thus the function
aorc() by default behaves like an step-up test.}
\item{betaAdjustment}{A numeric value to adjust the asymptotically optimal
rejection curve for the finite sample case. If betaAdjustment is not
set an algorithm will calculate it, but this can be time-consuming.}
\item{silent}{If true any output on the console will be suppressed.}}
\examples{r <- c(runif(10), runif(10, 0, 0.01))
result <- aorc(r, 0.05)
result <- aorc(r, 0.05, startIDX_SUD = 1)  ## step-down
result <- aorc(r, 0.05, startIDX_SUD = length(r))  ## step-up}

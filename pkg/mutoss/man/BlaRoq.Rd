\name{BlaRoq}
\alias{BlaRoq}
\title{Blanchard-Roquain (2008) step-up Procedure for arbitrary dependent p-Values...}
\usage{BlaRoq(pValues, alpha, pii, silent=FALSE)}
\description{Blanchard-Roquain (2008) step-up Procedure for arbitrary dependent p-Values
Also proposed independently by Sarkar (2008)}
\details{A generalization of the Benjamini-Yekutieli procedure, taking as an additional parameter
a distribution pii on [1..k] (k is the number of hypotheses)
representing prior belief on the number of hypotheses that will be rejected.

It is a step-up Procedure with critical values C_i defined as alpha/k times
the sum for j in [1..i] of j*pii[j]. For any fixed prior pii, the FDR is controlled at
level alpha for arbitrary dependence structure of the p-Values. The particular case of the
Benjamini-Yekutieli step-up is recovered by taking pii[i] proportional to 1/i.

If pii is missing, a default prior distribution proportional to exp( -i/(0.15*k) ) is taken.
It should perform better than the BY procedure if more than about 0.05 to 0.1 of hypotheses are rejected,
and worse otherwise.

Note: the procedure automatically normalizes the prior pii to sum to one if this is not the case.}
\value{A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{criticalValues}{A numeric vector containing critical values used in the step-up test} 

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}}
\author{GillesBlanchard,HackNiklas}
\references{Blanchard, G. and Roquain, E. (2008). Two simple sufficient conditions for FDR control.
Electronic Journal of Statistics, 2:963-992. 
Sarkar, S.K. (2008) On methods controlling the false discovery rate. 
Sankhya, Series A, 70:135-168.}
\arguments{\item{pValues}{pValues to be used. They can have arbitrary dependence.}
\item{alpha}{the level at which the FDR should be controlled}
\item{pii}{Prior for the proportion of true null hypotheses, same size as pValues}
\item{silent}{if true any output on the console will be suppressed.}}

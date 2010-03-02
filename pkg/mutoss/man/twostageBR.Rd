\name{twostageBR}
\alias{twostageBR}
\title{Blanchard-Roquain (2009) 2-stage adaptive step-up...}
\usage{twostageBR(pValues, alpha, lambda=1, silent=FALSE)}
\description{Blanchard-Roquain (2009) 2-stage adaptive step-up}
\details{This is an adaptive linear step-up procedure where the proportion of true
nulls is estimated using the Blanchard-Roquain 1-stage procedure with parameter lambda,
via the formula

estimated pi_0 = ( m - R(alpha,lambda)  + 1) / ( m*( 1 - lambda * alpha ) )

where R(alpha,lambda) is the number of hypotheses rejected by the BR 1-stage procedure,
alpha is the level at which FDR should be controlled and lambda an 
arbitrary parameter belonging to (0, 1/alpha) with default value 1.
This procedure controls FDR at the desired level when the p-values are independent.}
\value{A list containing:

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}}
\author{GillesBlanchard}
\references{Blanchard, G. and Roquain, E. (2009) 
Adaptive False Discovery Rate Control under Independence and Dependence
Journal of Machine Learning Research 10:2837-2871.}
\arguments{\item{pValues}{the used p-values (assumed to be independent)}
\item{alpha}{the level at which the FDR should be controlled.}
\item{lambda}{parameter of the procedure, should belong to
(0, 1/alpha) (lambda=1 default)}
\item{silent}{if true any output on the console will be suppressed.}}

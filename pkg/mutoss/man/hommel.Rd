\name{hommel}
\alias{hommel}
\title{Hommel's (1988) step-up-procedure}
\usage{hommel(pValues, alpha, silent=FALSE)}
\description{Hommel's step-up-procedure.}
\details{The method is applied to p-values. It controls
the FWER in the strong sense when the hypothesis tests are independent
or when they are non-negatively associated.

The method is based upon the closure principle and the Simes test.}
\value{A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test.}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}
}
\author{HackNiklas}
\references{
G. Hommel (1988). A stagewise rejective multiple test procedure based on a modified Bonferroni test. Biometrika 75, pp. 383-386}
\arguments{
\item{pValues}{pValues to be used. They need a independent structure.}
\item{alpha}{The level at which the FWER should be controlled}
\item{silent}{Logical. If true, any output on the console will be suppressed.}
}
\examples{
pval <- c(runif(50), runif(50, 0, 0.01))
result 	<- hommel(pval, 0.05)
result 	<- hommel(pval, 0.05, silent = TRUE)
}

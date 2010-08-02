\name{sidak}
\alias{sidak}
\title{Sidak correction}
\usage{sidak(pValues, alpha, silent=FALSE)}
\description{The classical Sidak correction returns adjusted p-values, ensuring strong FWER control under
the assumption of independence of the input p-values. It only uses the fact that the probability of no incorrect
rejection is the product over true nulls of those marginal probabilities (using the assumed independence of p-values).}
\details{The procedure is more generally valid for positive orthant dependent test statistics.

It is recommended to use the step-down version of the Sidak correction instead (see SidakSD), 
which is valid under the exact same assumptions but is more powerful.}
\value{A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{(if alpha is given) A logical vector indicating which hypotheses are rejected}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function.}}
\references{Sidak, Z. (1967). Rectangular confidence regions for the means of multivariate normal distributions.
Journal of the American Statistical Association, 62:626-633.}
\author{MuToss-Coding Team}
\arguments{
  \item{pValues}{pValues to be used.}
  \item{alpha}{The level at which the FWER shall be controlled (optional).}
  \item{silent}{logical scalar. If \code{TRUE} no output is generated.}
}
\examples{alpha <- 0.05
p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9, max=1))
result <- sidak(p)
result <- sidak(p, alpha)
result <- sidak(p, alpha, silent=TRUE)}

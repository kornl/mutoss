\name{storey_pi0_est}
\alias{storey_pi0_est}
\title{Storey-Taylor-Siegmund estimation of pi0 (finite sample version)}
\usage{storey_pi0_est(pValues, lambda)storey_pi0_est(pValues, lambda)
storey_pi0_est(pValues, lambda)
storey_pi0_est(pValues, lambda)
storey_pi0_est(pValues, lambda)
}
\description{The Storey-Taylor-Siegmund procedure for estimating pi0 is applied to pValues.
The formula is equivalent to that in Schweder and Spjotvoll (1982),
page 497, except the additional '+1' in the nominator that
introduces a conservative bias which is proven to be sufficiently large
for FDR control in finite families of hypotheses if the estimation
is used for adjusting the nominal level of a linear step-up test.}
\value{A list containing:

\item{pi0}{A numeric number containing the estimated value of pi0}

\item{lambda}{A numeric number containing the tuning parameter for the estimation}\code{storey_pi0_est}: A list containing:

\item{pi0}{A numeric number containing the estimated value of pi0}

\item{lambda}{A numeric number containing the tuning parameter for the estimation}


\code{storey_pi0_est}: A list containing:

\item{pi0}{A numeric number containing the estimated value of pi0}

\item{lambda}{A numeric number containing the tuning parameter for the estimation}


\code{storey_pi0_est}: A list containing:

\item{pi0}{A numeric number containing the estimated value of pi0}

\item{lambda}{A numeric number containing the tuning parameter for the estimation}


\code{storey_pi0_est}: A list containing:

\item{pi0}{A numeric number containing the estimated value of pi0}

\item{lambda}{A numeric number containing the tuning parameter for the estimation}


}
\author{MarselScheer}
\references{Schweder, T. and Spjotvoll, E. (1982). Plots of P-values to evaluate many tests simultaneously. 
Biometrika 69, 3, 493-502.

Storey, J. D., Taylor, J. E. and Siegmund, D. (2004). Strong control, conservative point estimation and
simultaneous conservative consistency of false discovery rates: a unified approach. JRSS B 66, 1, 187-205.Schweder, T. and Spjotvoll, E. (1982). Plots of P-values to evaluate many tests simultaneously. 
Biometrika 69, 3, 493-502.

Storey, J. D., Taylor, J. E. and Siegmund, D. (2004). Strong control, conservative point estimation and
simultaneous conservative consistency of false discovery rates: a unified approach. JRSS B 66, 1, 187-205.Schweder, T. and Spjotvoll, E. (1982). Plots of P-values to evaluate many tests simultaneously. 
Biometrika 69, 3, 493-502.

Storey, J. D., Taylor, J. E. and Siegmund, D. (2004). Strong control, conservative point estimation and
simultaneous conservative consistency of false discovery rates: a unified approach. JRSS B 66, 1, 187-205.Schweder, T. and Spjotvoll, E. (1982). Plots of P-values to evaluate many tests simultaneously. 
Biometrika 69, 3, 493-502.

Storey, J. D., Taylor, J. E. and Siegmund, D. (2004). Strong control, conservative point estimation and
simultaneous conservative consistency of false discovery rates: a unified approach. JRSS B 66, 1, 187-205.Schweder, T. and Spjotvoll, E. (1982). Plots of P-values to evaluate many tests simultaneously. 
Biometrika 69, 3, 493-502.

Storey, J. D., Taylor, J. E. and Siegmund, D. (2004). Strong control, conservative point estimation and
simultaneous conservative consistency of false discovery rates: a unified approach. JRSS B 66, 1, 187-205.}
\arguments{\item{pValues}{The raw p-values for the marginal test problems}
\item{lambda}{A tuning parameter in the interval (0, 1)}}
\examples{my.pvals <- c(runif(50), runif(50, 0, 0.01))
result <- storey_pi0_est(my.pvals, 0.5)my.pvals <- c(runif(50), runif(50, 0, 0.01))
result <- storey_pi0_est(my.pvals, 0.5)my.pvals <- c(runif(50), runif(50, 0, 0.01))
result <- storey_pi0_est(my.pvals, 0.5)my.pvals <- c(runif(50), runif(50, 0, 0.01))
result <- storey_pi0_est(my.pvals, 0.5)my.pvals <- c(runif(50), runif(50, 0, 0.01))
result <- storey_pi0_est(my.pvals, 0.5)}
\alias{storey_pi0_est}

\alias{storey_pi0_est}

\alias{storey_pi0_est}

\alias{storey_pi0_est}


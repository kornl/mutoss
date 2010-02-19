\name{ABH_pi0_est}
\alias{ABH_pi0_est}
\title{Lowest Slope Line (LSL) method of Hochberg and Benjamini for estimating pi0}
\usage{ABH_pi0_est(pValues)ABH_pi0_est(pValues)
ABH_pi0_est(pValues)
ABH_pi0_est(pValues)
ABH_pi0_est(pValues)
}
\description{The Lowest Slope Line (LSL) method of Hochberg and Benjamini for estimating pi0 is applied to pValues.
This method for estimating pi0 is motivated by the graphical approach proposed
by Schweder and Spjotvoll (1982), as developed and presented in Hochberg and Benjamini (1990).}
\value{\item{pi0.ABH}{The estimated proportion of true null hypotheses.}\code{ABH_pi0_est}: \item{pi0.ABH}{The estimated proportion of true null hypotheses.}


\code{ABH_pi0_est}: \item{pi0.ABH}{The estimated proportion of true null hypotheses.}


\code{ABH_pi0_est}: \item{pi0.ABH}{The estimated proportion of true null hypotheses.}


\code{ABH_pi0_est}: \item{pi0.ABH}{The estimated proportion of true null hypotheses.}


}
\author{WerftWiebke}
\references{Hochberg, Y. and Benjamini, Y. (1990). More powerful procedures for multiple significance testing. 
Statistics in Medicine 9, 811-818.

Schweder, T. and Spjotvoll, E. (1982). Plots of P-values to evaluate many tests simultaneously. 
Biometrika 69, 3, 493-502.Hochberg, Y. and Benjamini, Y. (1990). More powerful procedures for multiple significance testing. 
Statistics in Medicine 9, 811-818.

Schweder, T. and Spjotvoll, E. (1982). Plots of P-values to evaluate many tests simultaneously. 
Biometrika 69, 3, 493-502.Hochberg, Y. and Benjamini, Y. (1990). More powerful procedures for multiple significance testing. 
Statistics in Medicine 9, 811-818.

Schweder, T. and Spjotvoll, E. (1982). Plots of P-values to evaluate many tests simultaneously. 
Biometrika 69, 3, 493-502.Hochberg, Y. and Benjamini, Y. (1990). More powerful procedures for multiple significance testing. 
Statistics in Medicine 9, 811-818.

Schweder, T. and Spjotvoll, E. (1982). Plots of P-values to evaluate many tests simultaneously. 
Biometrika 69, 3, 493-502.Hochberg, Y. and Benjamini, Y. (1990). More powerful procedures for multiple significance testing. 
Statistics in Medicine 9, 811-818.

Schweder, T. and Spjotvoll, E. (1982). Plots of P-values to evaluate many tests simultaneously. 
Biometrika 69, 3, 493-502.}
\arguments{\item{pValues}{The raw p-values for the marginal test problems}

}
\examples{my.pvals <- c(runif(50), runif(50, 0, 0.01))
result <- ABH_pi0_est(my.pvals)my.pvals <- c(runif(50), runif(50, 0, 0.01))
result <- ABH_pi0_est(my.pvals)my.pvals <- c(runif(50), runif(50, 0, 0.01))
result <- ABH_pi0_est(my.pvals)my.pvals <- c(runif(50), runif(50, 0, 0.01))
result <- ABH_pi0_est(my.pvals)my.pvals <- c(runif(50), runif(50, 0, 0.01))
result <- ABH_pi0_est(my.pvals)}
\alias{ABH_pi0_est}

\alias{ABH_pi0_est}

\alias{ABH_pi0_est}

\alias{ABH_pi0_est}


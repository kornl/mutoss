\name{BH}
\alias{BH}
\title{Benjamini-Hochberg (1995) linear step-up procedure}
\usage{BH(pValues, alpha, silent=FALSE)BH(pValues, alpha, silent=FALSE)
BH(pValues, alpha, silent=FALSE)
BH(pValues, alpha, silent=FALSE)
BH(pValues, alpha, silent=FALSE)
}
\description{Benjamini-Hochbergs Linear Step-Up Procedure. 
The procedure controls the FDR when the test statistics are stochastically independent or satisfy positive regression dependency (PRDS) (see Benjamini and Yekutieli 2001 for details).
The Benjamini-Hochberg (BH) step-up procedure considers ordered pValues P_(i). 
It defines k as the largest i for which P_(i) <= i*alpha/m and then 
rejects all associated hypotheses H_(i) for i=1,...,k. In their seminal paper, Benjamini and Hochberg (1995) show 
that for 0 <= m_0 <= m independent pValues corresponding to true null hypotheses 
and for any joint distribution of the m_1 = m-m_0 p-values corresponding to the 
non null hypotheses, the FDR is controlled at level (m_0/m)*alpha.  Under the
assumption of the PRDS property, (for details see Benjamini and Yekutieli 2001). 
In Benjamini et al. (2006) the BH procedure is improved by adaptive procedures
which use an estimate of m_0 and apply the BH method al level alpha'=alpha*m/m_0, to 
fully exhaust the desired level alpha (see Adaptive Benjamini Hochberg and Two Stage Banjamini Yekutieli).}
\value{A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{criticalValues}{A numeric vector containing critical values used in the step-up test} 

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}\code{BH}: A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{criticalValues}{A numeric vector containing critical values used in the step-up test} 

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}


\code{BH}: A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{criticalValues}{A numeric vector containing critical values used in the step-up test} 

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}


\code{BH}: A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{criticalValues}{A numeric vector containing critical values used in the step-up test} 

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}


\code{BH}: A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{criticalValues}{A numeric vector containing critical values used in the step-up test} 

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}


}
\author{Werft Wiebke}
\references{Benjamini, Y. and Hochberg, Y. (1995). Controlling the false discovery rate: A practical and powerful approach to mulitple testing.
Journal of the Royal Statistical Society, Series B, 57:289-300.\eqn{n}

Benjamini, Y. and Yekutieli, D. (2001). The control of the false discovery rate in multiple testing under dependency.
Annals of Statistics, 29(4):1165-1188.\eqn{n}

Benjamini, Y., Krieger, A. and Yekutieli, D. (2006). Adaptive linear step-up procedures that control the false
discovery rate. Biometrika, 93(3):491-507.Benjamini, Y. and Hochberg, Y. (1995). Controlling the false discovery rate: A practical and powerful approach to mulitple testing.
Journal of the Royal Statistical Society, Series B, 57:289-300.\eqn{n}

Benjamini, Y. and Yekutieli, D. (2001). The control of the false discovery rate in multiple testing under dependency.
Annals of Statistics, 29(4):1165-1188.\eqn{n}

Benjamini, Y., Krieger, A. and Yekutieli, D. (2006). Adaptive linear step-up procedures that control the false
discovery rate. Biometrika, 93(3):491-507.Benjamini, Y. and Hochberg, Y. (1995). Controlling the false discovery rate: A practical and powerful approach to mulitple testing.
Journal of the Royal Statistical Society, Series B, 57:289-300.\eqn{n}

Benjamini, Y. and Yekutieli, D. (2001). The control of the false discovery rate in multiple testing under dependency.
Annals of Statistics, 29(4):1165-1188.\eqn{n}

Benjamini, Y., Krieger, A. and Yekutieli, D. (2006). Adaptive linear step-up procedures that control the false
discovery rate. Biometrika, 93(3):491-507.Benjamini, Y. and Hochberg, Y. (1995). Controlling the false discovery rate: A practical and powerful approach to mulitple testing.
Journal of the Royal Statistical Society, Series B, 57:289-300.\eqn{n}

Benjamini, Y. and Yekutieli, D. (2001). The control of the false discovery rate in multiple testing under dependency.
Annals of Statistics, 29(4):1165-1188.\eqn{n}

Benjamini, Y., Krieger, A. and Yekutieli, D. (2006). Adaptive linear step-up procedures that control the false
discovery rate. Biometrika, 93(3):491-507.Benjamini, Y. and Hochberg, Y. (1995). Controlling the false discovery rate: A practical and powerful approach to mulitple testing.
Journal of the Royal Statistical Society, Series B, 57:289-300.\eqn{n}

Benjamini, Y. and Yekutieli, D. (2001). The control of the false discovery rate in multiple testing under dependency.
Annals of Statistics, 29(4):1165-1188.\eqn{n}

Benjamini, Y., Krieger, A. and Yekutieli, D. (2006). Adaptive linear step-up procedures that control the false
discovery rate. Biometrika, 93(3):491-507.}
\arguments{\item{pValues}{The used unadjusted pValues.}
\item{alpha}{The level at which the FDR shall be controlled.}
\item{silent}{If true any output on the console will be suppressed.}}
\examples{alpha <- 0.05
p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9, max=1))
result <- BH(p, alpha)
result <- BH(p, alpha, silent=TRUE)alpha <- 0.05
p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9, max=1))
result <- BH(p, alpha)
result <- BH(p, alpha, silent=TRUE)alpha <- 0.05
p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9, max=1))
result <- BH(p, alpha)
result <- BH(p, alpha, silent=TRUE)alpha <- 0.05
p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9, max=1))
result <- BH(p, alpha)
result <- BH(p, alpha, silent=TRUE)alpha <- 0.05
p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9, max=1))
result <- BH(p, alpha)
result <- BH(p, alpha, silent=TRUE)}
\alias{BH}

\alias{BH}

\alias{BH}

\alias{BH}


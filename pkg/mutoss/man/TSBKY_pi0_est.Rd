\name{TSBKY_pi0_est}
\alias{TSBKY_pi0_est}
\title{Two-step estimation method of Benjamini, Krieger and Yekutieli for estimating pi0}
\usage{TSBKY_pi0_est(pValues, alpha)}
\description{The two-step estimation method of Benjamini, Krieger and Yekutieli for estimating pi0 is applied to pValues.
It consists of the following two steps:
Step 1. Use the linear step-up procedure at level alpha' =alpha/(1+alpha). Let r1 be the number of
rejected hypotheses. If r1=0 do not reject any hypothesis and stop; if r1=m reject all m
hypotheses and stop; otherwise continue.
Step 2. Let \eqn{\hat{m0} =(m - r1)} and \eqn{\hat{pi0} = \hat{m0} / m}.}
\value{\item{pi0.TSBKY}{The estimated proportion of true null hypotheses.}}
\author{WerftWiebke}
\references{Benjamini, Y., Krieger, A. and Yekutieli, D. (2006). Adaptive linear step-up procedures that control the false discovery rate
Biometrika 93, 3, page 495.}
\arguments{\item{pValues}{The raw p-values for the marginal test problems}
\item{alpha}{The parameter (to be interpreted as significance level) for the procedure}}
\examples{my.pvals <- c(runif(50), runif(50, 0, 0.01))
result <- TSBKY_pi0_est(my.pvals, 0.1)}

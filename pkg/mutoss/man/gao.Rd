\name{gao}
\alias{gao}
\title{Xin Gao's non-parametric multiple test procedure is applied to Data.}
\usage{gao(formula, data, alpha=0.05, control=NULL, silent=FALSE)}
\description{Xin Gao's non-parametric multiple test procedure is applied to Data. 
The procedure controls the FWER in the strong sense. Here, only the Many-To-One comparisons are computed.}
\details{This function computes Xin Gao's nonparametric multiple test procedures in an unbalanced one way layout. 
It is based upon the following purely nonparametric effects: 
Let \eqn{F_i} denote the distribution function of sample \eqn{i, i=1,\ldots,a,} and let \eqn{G} denote the mean distribution function
of all distribution functions \eqn{(G=1/a\sum_i F_i)}. The effects \eqn{p_i=\int GdF_i} are called unweighted relative effects. If \eqn{p_i>1/2}, the random
variables from sample \eqn{i} tend (stochastically) to larger values than any randomly chosen number 
from the whole experiment. If \eqn{p_i = 1/2}, there is no tendency to smaller nor larger values. However,
this approach tests the hypothesis \eqn{H_0^F: F_1=F_j, j=2,\ldots,a} formulated in terms of the
distribution functions, simultaneously.}
\value{A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{confIntervals}{A matrix containing the estimates and the lower and upper confidence bound}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}}
\author{Frank Konietschke}
\references{Gao, X. et al. (2008). Nonparametric multiple comparison procedures for unbalanced
one-way factorial designs. 
Journal of Statistical Planning and Inference 77, 2574-2591. \eqn{n}
The FWER is controlled by using the Hochberg adjustment
(Hochberg, Y. (1988). A sharper Bonferroni procedure for multiple tests of significance. Biometrika 75, 800-802.)}
\arguments{\item{formula}{Formula defining the statistical model, containing the response and the factors}
\item{data}{Dataset containing the response and the grouping factor}
\item{alpha}{The level at which the FWER shall be controlled. By default it is alpha=0.05.}
\item{silent}{If true any output on the console will be suppressed.}
\item{control}{The control group for the Many-To-One comparisons. By default it is the first group in lexicographical order.}}
\examples{x=c(rnorm(40))
f1=c(rep(1,10),rep(2,10),rep(3,10),rep(4,10))
my.data <- data.frame(x,f1)
result <- gao(x~f1,data=my.data, alpha=0.05,control=2, silent=FALSE)
result <- gao(x~f1,data=my.data, alpha=0.05,control=2, silent=TRUE)
result <- gao(x~f1,data=my.data, alpha=0.05)}

\name{regwq}
\alias{regwq}
\title{REGWQ - Ryan / Einot and Gabriel / Welsch  test procedure...}
\usage{regwq(formula, data, alpha, MSE=NULL, df=NULL, silent=FALSE)regwq(formula, data, alpha, MSE=NULL, df=NULL, silent=FALSE)
regwq(formula, data, alpha, MSE=NULL, df=NULL, silent=FALSE)
regwq(formula, data, alpha, MSE=NULL, df=NULL, silent=FALSE)
regwq(formula, data, alpha, MSE=NULL, df=NULL, silent=FALSE)
}
\description{REGWQ - Ryan / Einot and Gabriel / Welsch  test procedure
This function computes REGWQ test for given
data including p samples. It is based on a stepwise or 
layer approach to significance testing. Sample means are 
ordered from the smallest to the largest.  
The largest 
difference, which involves means that are r = p steps apart, 
is tested first at \eqn{\alpha} level of significance; if significant, 
means that are \eqn{r <p} steps apart are tested at a different \eqn{\alpha} level 
of significance and so on. Compare to the Student-
Newman-Keuls test, the \eqn{\alpha} levels are adjusted for the p-1 different
layers by the formula \eqn{\alpha_p=\alpha}, if p=k or p=k-1,
\eqn{\alpha_p = 1-(1-\alpha)^{p/k}}  otherwise. It might happen that the
quantiles are not descending in p. In this case, they are adapted by
\eqn{c_k = max_{2\leq r \leq k} c_r, k=2,\ldots,p}.
The REGWQ procedure, like Tukey's procedure, requires 
equal sample n's. However, in this algorithm, the procedure is 
adapted to unequal sample sized which can lead to still 
conservative test decisions.}
\value{A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{statistics}{A numeric vector containing the test-statistics}

\item{confIntervals}{A matrix containing only the estimates}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function.}\code{regwq}: A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{statistics}{A numeric vector containing the test-statistics}

\item{confIntervals}{A matrix containing only the estimates}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function.}


\code{regwq}: A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{statistics}{A numeric vector containing the test-statistics}

\item{confIntervals}{A matrix containing only the estimates}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function.}


\code{regwq}: A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{statistics}{A numeric vector containing the test-statistics}

\item{confIntervals}{A matrix containing only the estimates}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function.}


\code{regwq}: A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{statistics}{A numeric vector containing the test-statistics}

\item{confIntervals}{A matrix containing only the estimates}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function.}


}
\author{Frank Konietschke}
\references{Hochberg, Y. & Tamhane, A. C. (1987). Multiple Comparison Procedures,  Wiley.Hochberg, Y. & Tamhane, A. C. (1987). Multiple Comparison Procedures,  Wiley.Hochberg, Y. & Tamhane, A. C. (1987). Multiple Comparison Procedures,  Wiley.Hochberg, Y. & Tamhane, A. C. (1987). Multiple Comparison Procedures,  Wiley.Hochberg, Y. & Tamhane, A. C. (1987). Multiple Comparison Procedures,  Wiley.}
\arguments{\item{formula}{Formula defining the statistical model containing the response and the factors}
\item{data}{dataset containing the response and the grouping factor}
\item{alpha}{The level at which the error should be controlled. By default it is alpha=0.05.}
\item{MSE}{Optional for a given variance of the data}
\item{df}{Optional for a given degree of freedom}
\item{silent}{If true any output on the console will be suppressed.}}
\examples{x = rnorm(50)
grp = c(rep(1:5,10))
dataframe <- data.frame(x,grp)
result <- regwq(x~grp, data=dataframe, alpha=0.05,MSE=NULL, df=NULL, silent = TRUE)
result <- regwq(x~grp, data=dataframe, alpha=0.05,MSE=NULL, df=NULL, silent = FALSE)
result <- regwq(x~grp, data=dataframe, alpha=0.05,MSE=1, df=Inf, silent = FALSE) # known variance
result <- regwq(x~grp, data=dataframe, alpha=0.05,MSE=1, df=1000, silent = FALSE) # known variancex = rnorm(50)
grp = c(rep(1:5,10))
dataframe <- data.frame(x,grp)
result <- regwq(x~grp, data=dataframe, alpha=0.05,MSE=NULL, df=NULL, silent = TRUE)
result <- regwq(x~grp, data=dataframe, alpha=0.05,MSE=NULL, df=NULL, silent = FALSE)
result <- regwq(x~grp, data=dataframe, alpha=0.05,MSE=1, df=Inf, silent = FALSE) # known variance
result <- regwq(x~grp, data=dataframe, alpha=0.05,MSE=1, df=1000, silent = FALSE) # known variancex = rnorm(50)
grp = c(rep(1:5,10))
dataframe <- data.frame(x,grp)
result <- regwq(x~grp, data=dataframe, alpha=0.05,MSE=NULL, df=NULL, silent = TRUE)
result <- regwq(x~grp, data=dataframe, alpha=0.05,MSE=NULL, df=NULL, silent = FALSE)
result <- regwq(x~grp, data=dataframe, alpha=0.05,MSE=1, df=Inf, silent = FALSE) # known variance
result <- regwq(x~grp, data=dataframe, alpha=0.05,MSE=1, df=1000, silent = FALSE) # known variancex = rnorm(50)
grp = c(rep(1:5,10))
dataframe <- data.frame(x,grp)
result <- regwq(x~grp, data=dataframe, alpha=0.05,MSE=NULL, df=NULL, silent = TRUE)
result <- regwq(x~grp, data=dataframe, alpha=0.05,MSE=NULL, df=NULL, silent = FALSE)
result <- regwq(x~grp, data=dataframe, alpha=0.05,MSE=1, df=Inf, silent = FALSE) # known variance
result <- regwq(x~grp, data=dataframe, alpha=0.05,MSE=1, df=1000, silent = FALSE) # known variancex = rnorm(50)
grp = c(rep(1:5,10))
dataframe <- data.frame(x,grp)
result <- regwq(x~grp, data=dataframe, alpha=0.05,MSE=NULL, df=NULL, silent = TRUE)
result <- regwq(x~grp, data=dataframe, alpha=0.05,MSE=NULL, df=NULL, silent = FALSE)
result <- regwq(x~grp, data=dataframe, alpha=0.05,MSE=1, df=Inf, silent = FALSE) # known variance
result <- regwq(x~grp, data=dataframe, alpha=0.05,MSE=1, df=1000, silent = FALSE) # known variance}
\alias{regwq}

\alias{regwq}

\alias{regwq}

\alias{regwq}


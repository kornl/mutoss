\name{snk}
\alias{snk}
\title{Student - Newman - Keuls rejective test procedure.}
\usage{snk(formula, data, alpha, MSE=NULL, df=NULL, silent=FALSE)snk(formula, data, alpha, MSE=NULL, df=NULL, silent=FALSE)
snk(formula, data, alpha, MSE=NULL, df=NULL, silent=FALSE)
snk(formula, data, alpha, MSE=NULL, df=NULL, silent=FALSE)
snk(formula, data, alpha, MSE=NULL, df=NULL, silent=FALSE)
}
\description{Student - Newman - Keuls rejective test procedure.
The procedure controls the FWER in the WEAK sense.}
\details{This function computes the Student-Newman-Keuls test for given
data including p samples. The Newman-Keuls procedure is based on a stepwise or 
layer approach to significance testing. Sample means are 
ordered from the smallest to the largest. The largest 
difference, which involves means that are r = p steps apart, 
is tested first at \eqn{\alpha} level of significance; if significant, 
means that are r = p - 1 steps apart are tested at \eqn{\alpha} level 
of significance and so on. The Newman-Keuls procedure provides an
r-mean significance level equal to \eqn{\alpha} for each group of 
r ordered means, that is, the probability of falsely rejecting 
the hypothesis that all means in an ordered group are equal to 
\eqn{\alpha}. It follows that the concept of error rate applies 
neither on an experimentwise nor on a per comparison basis-the
actual error rate falls somewhere between the two. 
The Newman-Keuls procedure, like Tukey's procedure, requires 
equal sample n's. However, in this algorithm, the procedure is 
adapted to unequal sample sized which can lead to still 
conservative test decisions. 

It should be noted that the Newman-Keuls and Tukey procedures 
require the same critical difference for the first comparison 
that is tested. The Tukey procedure uses this critical difference
for all the remaining tests, whereas the Newman-Keuls procedure 
reduces the size of the critical difference, depending on the 
number of steps separating the ordered means. As a result, the 
Newman-Keuls test is more powerful than Tukey's test. Remember,
however, that the Newman-Keuls procedure does not control the 
experimentwise error rate at \eqn{\alpha}.\code{snk}: This function computes the Student-Newman-Keuls test for given
data including p samples. The Newman-Keuls procedure is based on a stepwise or 
layer approach to significance testing. Sample means are 
ordered from the smallest to the largest. The largest 
difference, which involves means that are r = p steps apart, 
is tested first at \eqn{\alpha} level of significance; if significant, 
means that are r = p - 1 steps apart are tested at \eqn{\alpha} level 
of significance and so on. The Newman-Keuls procedure provides an
r-mean significance level equal to \eqn{\alpha} for each group of 
r ordered means, that is, the probability of falsely rejecting 
the hypothesis that all means in an ordered group are equal to 
\eqn{\alpha}. It follows that the concept of error rate applies 
neither on an experimentwise nor on a per comparison basis-the
actual error rate falls somewhere between the two. 
The Newman-Keuls procedure, like Tukey's procedure, requires 
equal sample n's. However, in this algorithm, the procedure is 
adapted to unequal sample sized which can lead to still 
conservative test decisions. 

It should be noted that the Newman-Keuls and Tukey procedures 
require the same critical difference for the first comparison 
that is tested. The Tukey procedure uses this critical difference
for all the remaining tests, whereas the Newman-Keuls procedure 
reduces the size of the critical difference, depending on the 
number of steps separating the ordered means. As a result, the 
Newman-Keuls test is more powerful than Tukey's test. Remember,
however, that the Newman-Keuls procedure does not control the 
experimentwise error rate at \eqn{\alpha}.


\code{snk}: This function computes the Student-Newman-Keuls test for given
data including p samples. The Newman-Keuls procedure is based on a stepwise or 
layer approach to significance testing. Sample means are 
ordered from the smallest to the largest. The largest 
difference, which involves means that are r = p steps apart, 
is tested first at \eqn{\alpha} level of significance; if significant, 
means that are r = p - 1 steps apart are tested at \eqn{\alpha} level 
of significance and so on. The Newman-Keuls procedure provides an
r-mean significance level equal to \eqn{\alpha} for each group of 
r ordered means, that is, the probability of falsely rejecting 
the hypothesis that all means in an ordered group are equal to 
\eqn{\alpha}. It follows that the concept of error rate applies 
neither on an experimentwise nor on a per comparison basis-the
actual error rate falls somewhere between the two. 
The Newman-Keuls procedure, like Tukey's procedure, requires 
equal sample n's. However, in this algorithm, the procedure is 
adapted to unequal sample sized which can lead to still 
conservative test decisions. 

It should be noted that the Newman-Keuls and Tukey procedures 
require the same critical difference for the first comparison 
that is tested. The Tukey procedure uses this critical difference
for all the remaining tests, whereas the Newman-Keuls procedure 
reduces the size of the critical difference, depending on the 
number of steps separating the ordered means. As a result, the 
Newman-Keuls test is more powerful than Tukey's test. Remember,
however, that the Newman-Keuls procedure does not control the 
experimentwise error rate at \eqn{\alpha}.


\code{snk}: This function computes the Student-Newman-Keuls test for given
data including p samples. The Newman-Keuls procedure is based on a stepwise or 
layer approach to significance testing. Sample means are 
ordered from the smallest to the largest. The largest 
difference, which involves means that are r = p steps apart, 
is tested first at \eqn{\alpha} level of significance; if significant, 
means that are r = p - 1 steps apart are tested at \eqn{\alpha} level 
of significance and so on. The Newman-Keuls procedure provides an
r-mean significance level equal to \eqn{\alpha} for each group of 
r ordered means, that is, the probability of falsely rejecting 
the hypothesis that all means in an ordered group are equal to 
\eqn{\alpha}. It follows that the concept of error rate applies 
neither on an experimentwise nor on a per comparison basis-the
actual error rate falls somewhere between the two. 
The Newman-Keuls procedure, like Tukey's procedure, requires 
equal sample n's. However, in this algorithm, the procedure is 
adapted to unequal sample sized which can lead to still 
conservative test decisions. 

It should be noted that the Newman-Keuls and Tukey procedures 
require the same critical difference for the first comparison 
that is tested. The Tukey procedure uses this critical difference
for all the remaining tests, whereas the Newman-Keuls procedure 
reduces the size of the critical difference, depending on the 
number of steps separating the ordered means. As a result, the 
Newman-Keuls test is more powerful than Tukey's test. Remember,
however, that the Newman-Keuls procedure does not control the 
experimentwise error rate at \eqn{\alpha}.


\code{snk}: This function computes the Student-Newman-Keuls test for given
data including p samples. The Newman-Keuls procedure is based on a stepwise or 
layer approach to significance testing. Sample means are 
ordered from the smallest to the largest. The largest 
difference, which involves means that are r = p steps apart, 
is tested first at \eqn{\alpha} level of significance; if significant, 
means that are r = p - 1 steps apart are tested at \eqn{\alpha} level 
of significance and so on. The Newman-Keuls procedure provides an
r-mean significance level equal to \eqn{\alpha} for each group of 
r ordered means, that is, the probability of falsely rejecting 
the hypothesis that all means in an ordered group are equal to 
\eqn{\alpha}. It follows that the concept of error rate applies 
neither on an experimentwise nor on a per comparison basis-the
actual error rate falls somewhere between the two. 
The Newman-Keuls procedure, like Tukey's procedure, requires 
equal sample n's. However, in this algorithm, the procedure is 
adapted to unequal sample sized which can lead to still 
conservative test decisions. 

It should be noted that the Newman-Keuls and Tukey procedures 
require the same critical difference for the first comparison 
that is tested. The Tukey procedure uses this critical difference
for all the remaining tests, whereas the Newman-Keuls procedure 
reduces the size of the critical difference, depending on the 
number of steps separating the ordered means. As a result, the 
Newman-Keuls test is more powerful than Tukey's test. Remember,
however, that the Newman-Keuls procedure does not control the 
experimentwise error rate at \eqn{\alpha}.


}
\value{A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{statistics}{A numeric vector containing the test-statistics}

\item{confIntervals}{A matrix containing only the estimates}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function.}\code{snk}: A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{statistics}{A numeric vector containing the test-statistics}

\item{confIntervals}{A matrix containing only the estimates}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function.}


\code{snk}: A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{statistics}{A numeric vector containing the test-statistics}

\item{confIntervals}{A matrix containing only the estimates}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function.}


\code{snk}: A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{statistics}{A numeric vector containing the test-statistics}

\item{confIntervals}{A matrix containing only the estimates}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function.}


\code{snk}: A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{statistics}{A numeric vector containing the test-statistics}

\item{confIntervals}{A matrix containing only the estimates}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function.}


}
\author{Frank Konietschke}
\references{Keuls M (1952). "The use of the studentized range in connection with an analysis of variance". Euphytica 1: 112-122Keuls M (1952). "The use of the studentized range in connection with an analysis of variance". Euphytica 1: 112-122Keuls M (1952). "The use of the studentized range in connection with an analysis of variance". Euphytica 1: 112-122Keuls M (1952). "The use of the studentized range in connection with an analysis of variance". Euphytica 1: 112-122Keuls M (1952). "The use of the studentized range in connection with an analysis of variance". Euphytica 1: 112-122}
\arguments{\item{formula}{Formula defining the statistical model containing the response and the factor levels.}
\item{data}{dataset containing the response and the grouping factor.}
\item{alpha}{The level at which the error should be controlled. By default it is alpha=0.05.}
\item{MSE}{Optional for a given variance of the data.}
\item{df}{Optional for a given degree of freedom.}
\item{silent}{If true any output on the console will be suppressed.}}
\examples{x = rnorm(50)
grp = c(rep(1:5,10))
dataframe <- data.frame(x,grp)
result <- snk(x~grp, data=dataframe, alpha=0.05,MSE=NULL, df=NULL, silent = TRUE)
result <- snk(x~grp, data=dataframe,alpha=0.05,MSE=NULL, df=NULL, silent = FALSE)
result <- snk(x~grp, data=dataframe,alpha=0.05,MSE=1, df=Inf, silent = FALSE) # known variance
result <- snk(x~grp, data=dataframe,alpha=0.05,MSE=1, df=1000, silent = FALSE) # known variancex = rnorm(50)
grp = c(rep(1:5,10))
dataframe <- data.frame(x,grp)
result <- snk(x~grp, data=dataframe, alpha=0.05,MSE=NULL, df=NULL, silent = TRUE)
result <- snk(x~grp, data=dataframe,alpha=0.05,MSE=NULL, df=NULL, silent = FALSE)
result <- snk(x~grp, data=dataframe,alpha=0.05,MSE=1, df=Inf, silent = FALSE) # known variance
result <- snk(x~grp, data=dataframe,alpha=0.05,MSE=1, df=1000, silent = FALSE) # known variancex = rnorm(50)
grp = c(rep(1:5,10))
dataframe <- data.frame(x,grp)
result <- snk(x~grp, data=dataframe, alpha=0.05,MSE=NULL, df=NULL, silent = TRUE)
result <- snk(x~grp, data=dataframe,alpha=0.05,MSE=NULL, df=NULL, silent = FALSE)
result <- snk(x~grp, data=dataframe,alpha=0.05,MSE=1, df=Inf, silent = FALSE) # known variance
result <- snk(x~grp, data=dataframe,alpha=0.05,MSE=1, df=1000, silent = FALSE) # known variancex = rnorm(50)
grp = c(rep(1:5,10))
dataframe <- data.frame(x,grp)
result <- snk(x~grp, data=dataframe, alpha=0.05,MSE=NULL, df=NULL, silent = TRUE)
result <- snk(x~grp, data=dataframe,alpha=0.05,MSE=NULL, df=NULL, silent = FALSE)
result <- snk(x~grp, data=dataframe,alpha=0.05,MSE=1, df=Inf, silent = FALSE) # known variance
result <- snk(x~grp, data=dataframe,alpha=0.05,MSE=1, df=1000, silent = FALSE) # known variancex = rnorm(50)
grp = c(rep(1:5,10))
dataframe <- data.frame(x,grp)
result <- snk(x~grp, data=dataframe, alpha=0.05,MSE=NULL, df=NULL, silent = TRUE)
result <- snk(x~grp, data=dataframe,alpha=0.05,MSE=NULL, df=NULL, silent = FALSE)
result <- snk(x~grp, data=dataframe,alpha=0.05,MSE=1, df=Inf, silent = FALSE) # known variance
result <- snk(x~grp, data=dataframe,alpha=0.05,MSE=1, df=1000, silent = FALSE) # known variance}
\alias{snk}

\alias{snk}

\alias{snk}

\alias{snk}


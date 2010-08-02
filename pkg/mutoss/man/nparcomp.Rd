\name{nparcomp}
\alias{nparcomp}
\title{Simultaneous confidence intervals for relative contrast effects...}
\usage{nparcomp(formula, data, type=c("UserDefined", "Tukey", "Dunnett",
    "Sequen", "Williams", "Changepoint", "AVE", "McDermott", "Marcus",
    "UmbrellaWilliams"), control=NULL, conflevel=0.95,
    alternative=c("two.sided", "less", "greater"), rounds=3,
    correlation=FALSE, asy.method=c("logit", "probit", "normal",
    "mult.t"), plot.simci=FALSE, info=TRUE, contrastMatrix=NULL)}
\description{Simultaneous confidence intervals for relative contrast effects
The procedure controls the FWER in the strong sense.}
\details{With this function, it is possible to compute nonparametric simultaneous confidence
intervals for relative contrast effects in the unbalanced one way layout. Moreover, it computes
adjusted p-values. The simultaneous confidence intervals can be computed using
multivariate normal distribution, multivariate t-distribution with a Satterthwaite Approximation
of the degree of freedom or using multivariate range preserving transformations with Logit or
Probit as transformation function. There is no assumption on the underlying distribution function, only
that the data have to be at least ordinal numbers}
\value{A list containing:  
\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{confIntervals}{A matrix containing the estimates and the lower and upper confidence bound}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function.}}
\author{FrankKonietschke}
\arguments{\item{formula}{A two-sided 'formula' specifying a numeric response variable and
a factor with more than two levels. If the factor contains less than 3 levels, an
error message will be returned}
\item{data}{data A dataframe containing the variables specified in formula}
\item{type}{type Character string defining the type of contrast. It should be one of "Tukey",
"Dunnett", "Sequen", "Williams", "Changepoint", "AVE", "McDermott", "Marcus"}
\item{control}{control Character string defining the control group in Dunnett comparisons.
By default it is the first group by lexicographical ordering}
\item{conflevel}{The confidence level for the 1 - conflevel confidence intervals. By
default it is 0.05}
\item{alternative}{Character string defining the alternative hypothesis, one of
"two.sided", "less" or "greater"}
\item{rounds}{Number of rounds for the numeric values of the output. By default it is rounds=3}
\item{correlation}{Correlation A logical whether the estimated correlation matrix and covariance
matrix should be printed}
\item{asy.method}{asy.method character string defining the asymptotic approximation method,
one of "logit", for using the logit transformation function, "probit", for using
the probit transformation function, "normal", for using the multivariate normal
distribution or "mult.t" for using a multivariate t-distribution with a Satterthwaite
Approximation}
\item{plot.simci}{plot.simci A logical indicating whether you want a plot of the confidence
intervals}
\item{info}{info A logical whether you want a brief overview with informations about the
output}
\item{contrastMatrix}{arbitrary contrast matrix given by the user}}
\examples{
\dontrun{# TODO Check this example and set a seed!
grp <- rep(1:5,10)
x <- rnorm(50, grp)
dataframe <- data.frame(x,grp)
# Williams Contrast
nparcomp(x ~grp, data=dataframe, asy.method = "probit",
type = "Williams", alternative = "two.sided", plot.simci = TRUE, info = TRUE)

# Dunnett Contrast
nparcomp(x ~grp, data=dataframe, asy.method = "probit",control=1,
type = "Dunnett", alternative = "two.sided", plot.simci = TRUE, info = TRUE)

# Dunnett dose 3 is baseline
nparcomp(x ~grp, data=dataframe, asy.method = "probit",
type = "Dunnett", control = "3",alternative = "two.sided",
plot.simci = TRUE, info = TRUE)
}
}

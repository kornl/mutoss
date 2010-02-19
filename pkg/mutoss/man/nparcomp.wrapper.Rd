\name{nparcomp.wrapper}
\alias{nparcomp.wrapper}
\title{Simultaneous confidence intervals for relative contrast effects...}
\usage{nparcomp.wrapper(model, data, hypotheses, alpha, alternative,
    asy.method)nparcomp.wrapper(model, data, hypotheses, alpha, alternative, asy.method)
nparcomp.wrapper(model, data, hypotheses, alpha, alternative, asy.method)
nparcomp.wrapper(model, data, hypotheses, alpha, alternative, asy.method)
nparcomp.wrapper(model, data, hypotheses, alpha, alternative, asy.method)
}
\description{Simultaneous confidence intervals for relative contrast effects
The procedure controls the FWER in the strong sense.}
\details{With this function, it is possible to compute nonparametric simultaneous confidence
intervals for relative contrast effects in the unbalanced one way layout. Moreover, it computes
adjusted p-values. The simultaneous confidence intervals can be computed using
multivariate normal distribution, multivariate t-distribution with a Satterthwaite Approximation
of the degree of freedom or using multivariate range preserving transformations with Logit or
Probit as transformation function. There is no assumption on the underlying distribution function, only
that the data have to be at least ordinal numbers\code{nparcomp.wrapper}: With this function, it is possible to compute nonparametric simultaneous confidence
intervals for relative contrast effects in the unbalanced one way layout. Moreover, it computes
adjusted p-values. The simultaneous confidence intervals can be computed using
multivariate normal distribution, multivariate t-distribution with a Satterthwaite Approximation
of the degree of freedom or using multivariate range preserving transformations with Logit or
Probit as transformation function. There is no assumption on the underlying distribution function, only
that the data have to be at least ordinal numbers


\code{nparcomp.wrapper}: With this function, it is possible to compute nonparametric simultaneous confidence
intervals for relative contrast effects in the unbalanced one way layout. Moreover, it computes
adjusted p-values. The simultaneous confidence intervals can be computed using
multivariate normal distribution, multivariate t-distribution with a Satterthwaite Approximation
of the degree of freedom or using multivariate range preserving transformations with Logit or
Probit as transformation function. There is no assumption on the underlying distribution function, only
that the data have to be at least ordinal numbers


\code{nparcomp.wrapper}: With this function, it is possible to compute nonparametric simultaneous confidence
intervals for relative contrast effects in the unbalanced one way layout. Moreover, it computes
adjusted p-values. The simultaneous confidence intervals can be computed using
multivariate normal distribution, multivariate t-distribution with a Satterthwaite Approximation
of the degree of freedom or using multivariate range preserving transformations with Logit or
Probit as transformation function. There is no assumption on the underlying distribution function, only
that the data have to be at least ordinal numbers


}
\value{A list containing:  
\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{confIntervals}{A matrix containing the estimates and the lower and upper confidence bound}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function.}\code{nparcomp.wrapper}: A list containing:  
\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{confIntervals}{A matrix containing the estimates and the lower and upper confidence bound}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function.}


\code{nparcomp.wrapper}: A list containing:  
\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{confIntervals}{A matrix containing the estimates and the lower and upper confidence bound}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function.}


\code{nparcomp.wrapper}: A list containing:  
\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{confIntervals}{A matrix containing the estimates and the lower and upper confidence bound}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function.}


}
\author{FrankKonietschke}
\arguments{\item{model}{A two-sided ’formula’ specifying a numeric response variable and
a factor with more than two levels.}
\item{data}{A dataframe containing the variables specified the model}
\item{hypotheses}{Character string defining the type of contrast. It should be one of "Tukey",
"Dunnett", "Sequen", "Williams", "Changepoint", "AVE", "McDermott", "Marcus".}
\item{alpha}{the significance level}
\item{alternative}{Character string defining the alternative hypothesis, one of
"two.sided", "less" or "greater"}
\item{asy.method}{A character string defining the asymptotic approximation method,
one of "logit", for using the logit transformation function, "probit", for using
the probit transformation function, "normal", for using the multivariate normal}}
\alias{nparcomp.wrapper}

\alias{nparcomp.wrapper}

\alias{nparcomp.wrapper}

\alias{nparcomp.wrapper}


\name{tukey.wrapper}
\alias{tukey.wrapper}
\title{Tukey HSD test and simultaneous confidence intervals for all pairs comparisons...}
\usage{tukey.wrapper(model, alpha, factorC)}
\description{Tukey HSD test and simultaneous confidence intervals for all pairs comparisons
in factorial designs. The procedure controls the FWER in the strong sense.}
\details{this function, it is possible to compute all pairs comparisons for expectations and 
simultaneous confidence intervals in factorial linear models. Hereby, the all-pairs comparisons
can be performed for user given effects. The overall variance is estimated by the linear model
as well as the degree of freedom used by the studentized range distribution.}
\value{A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{confIntervals}{A matrix containing the estimates and the lower and upper confidence bound}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function.}}
\author{Frank Konietschke et al.}
\arguments{\item{model}{A fitted model, for example an object returned by lm, glm, or aov etc. It is
assumed that coef and vcov methods are available for model. Usually, it is an aov fit}
\item{alpha}{The significance level}
\item{factorC}{Specifies a factor}}
\examples{data(warpbreaks)
# Tukey contrast on the levels of the factor "Tension"

tukey.wrapper(aov(breaks ~ tension, data = warpbreaks), factorC="tension",alpha=0.05)


# Two-way anova with interaction
tukey.wrapper(aov(breaks ~ tension*wool, data = warpbreaks),alpha=0.05,factorC="tension")
# Two-way anova without interaction

tukey.wrapper(aov(breaks ~ tension+wool, data = warpbreaks),alpha=0.05,factorC="tension")
tukey.wrapper(aov(breaks ~ tension, data = warpbreaks),alpha=0.05,factorC="tension")


data(iris)
tukey.wrapper(lm(Sepal.Length ~ Species, data=iris),alpha=0.05, factorC="Species")}

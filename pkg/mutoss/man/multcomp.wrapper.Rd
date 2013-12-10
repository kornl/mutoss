\name{multcomp.wrapper}
\alias{multcomp.wrapper}
\title{Simultaneous confidence intervals for arbitrary parametric contrasts in unbalanced one-way layouts.}
\usage{multcomp.wrapper(model, hypotheses, alternative, rhs=0, alpha, factorC)}
\description{Simultaneous confidence intervals for arbitrary parametric contrasts in unbalanced one-way layouts.
The procedure controls the FWER in the strong sense.}
\details{this function, it is possible to compute simultaneous confidence
for arbitrary parametric contrasts in the unbalanced one way layout. Moreover, it computes
p-values. The simultaneous confidence intervals are computed using
multivariate t-distribution.}
\value{A list containing:  
\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{confIntervals}{A matrix containing the estimates and the lower and upper confidence bound}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function.}}
\author{MuToss-Coding Team}
\arguments{\item{model}{a fitted model, for example an object returned by lm, glm, or aov etc. It is
assumed that coef and vcov methods are available for model.}
\item{hypotheses}{a specification of the linear hypotheses to be tested.}
\item{alternative}{a character string specifying the alternative hypothesis, must be one of 'two.sided'
(default), 'greater' or 'less'.}
\item{rhs}{an optional numeric vector specifying the right hand side of the hypothesis.}
\item{alpha}{the significance level}
\item{factorC}{character string, specifing the factor variable of interest}
}
\examples{data(warpbreaks)
# Tukey contrast on the levels of the factor 'Tension'

multcomp.wrapper(aov(breaks ~ tension, data = warpbreaks), 
  hypotheses = "Tukey", alternative="two.sided", factorC="tension",alpha=0.05)

# Williams contrast on 'Tension'
multcomp.wrapper(aov(breaks ~ tension, data = warpbreaks), 
  hypotheses= "Williams", alternative="two.sided",alpha=0.05,factorC="tension")

# Userdefined contrast matrix
K <-matrix(c(-1,0,1,-1,1,0, -1,0.5,0.5),ncol=3,nrow=3,byrow=TRUE)
multcomp.wrapper(aov(breaks ~ tension, data = warpbreaks), 
  hypotheses=K, alternative="two.sided",alpha=0.05,factorC="tension")

# Two-way anova
multcomp.wrapper(aov(breaks ~ tension*wool, data = warpbreaks), 
  hypotheses="Tukey", alternative="two.sided",alpha=0.05,factorC="wool")
multcomp.wrapper(aov(breaks ~ tension*wool, data = warpbreaks), 
  hypotheses="Tukey", alternative="two.sided",alpha=0.05,factorC="tension")
multcomp.wrapper(aov(breaks ~ tension*wool, data = warpbreaks), 
  hypotheses=K, alternative="two.sided",alpha=0.05, factorC="tension")
data(iris)
multcomp.wrapper(model=lm(Sepal.Length ~ Species, data=iris), 
  hypotheses="Tukey","two.sided",alpha=0.05, factorC="Species")
K <-matrix(c(-1,0,1,-1,1,0, -1,0.5,0.5),ncol=3,nrow=3,byrow=TRUE)
multcomp.wrapper(model=lm(Sepal.Length ~ Species, data=iris),
  hypotheses=K,"two.sided",alpha=0.05, factorC="Species")}

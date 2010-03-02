\name{mutoss.apply}
\alias{mutoss.apply}
\title{Applies a function to a Mutoss object.}
\usage{mutoss.apply(mutossObj, f, label, ...)}
\description{Applies a function to a Mutoss object.}
\details{This functions is intended for applying functions for multiplicity control on Mutoss class objects using the console and not the Mutoss GUI.}
\value{A Mutoss object after applying the given function

\item{mutossObj}{Object of S4 class Mutoss}}
\author{Kornelius Rohmeyer}
\arguments{\item{mutossObj}{the Mutoss object the function should be applied to}
\item{f}{the function that should be applied}
\item{label}{the label all slots of the Mutoss object should get that where changed}
\item{...}{additional parameters that are passed to the function}}
\examples{newObjectBonf <- mutoss.apply(new(Class="Mutoss", pValues=runif(10)), f=bonferroni, label="Bonferroni Correction", alpha=0.05)
\dontrun{ TODO: EXAMPLE PROBLEM
newObjectHolm <- mutoss.apply(new(Class="Mutoss", pValues=runif(10)), f=holm, label="Holm's step-down-procedure", alpha=0.05, silent=T)

newObjectAORC <- mutoss.apply(new(Class="Mutoss", pValues=runif(10)), f=aorc, label="Asymtotically optimal rejection curve", alpha=0.05, startIDX_SUD = 1, silent=T)
}}

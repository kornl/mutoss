\name{gatherParameters}
\alias{gatherParameters}
\title{Extracts the parameters from the simulation()-Object}
\usage{gatherParameters(simObject)}
\description{Basically, it is a helper function for gatherStatistics(). 
It extracts the parameters from the simObject$results and
creates a data.frame from this. Every used parameter gets its own column.
Every row corresponds to one object in simObject$results. If a object A from
simObject$results does not contain a parameter P that another object B does, 
then the row for object A will have "" in the column for the parameter P.}
\value{A data.frame with rows for every object in simObject$results and columns for
the used parameter.}
\author{MarselScheer}
\arguments{\item{simObject}{An object returned by simulation()}}
\examples{#' # this function generates pValues 
myGen <- function(n, n0) {
  list(procInput=list(pValues = c(runif(n-n0, 0, 0.01), 
       runif(n0))), groundTruth = c(rep(FALSE, times=n-n0), rep(TRUE, times=n0)))
}

# need some simulation()-Object I can work with 
sim <- simulation(replications = 3, list(funName="myGen", fun=myGen, n=200, n0=c(50,100)), 
list(list(funName="BH", fun=function(pValues, alpha) BH(pValues, alpha, silent=TRUE), 
  alpha=c(0.25, 0.5)),
list(funName="holm", fun=holm, alpha=c(0.25, 0.5),silent=TRUE)))

gatherParameters(sim)}
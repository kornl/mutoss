\name{gatherStatistics}
\alias{gatherStatistics}
\title{Gathering statistics from simulation()-Object}
\usage{gatherStatistics(simObject, listOfStatisticFunctions,
    listOfAvgFunctions)}
\description{This function facilitates gathering statistics from the object returned by simulation().}
\details{For every simulation()-Object in simObject$results all statistics in listOfStatisticFunctions are calculated. 
If in addition listOfAvgFunctions is provided then the statistics of the objects that have the same
parameter constellation are averaged by the functions in listOfAvgFunctions. 
The resulting data.frame will then keep only one row for every parameter constellation.}
\value{\item{statisticDF}{A data.frame that can be of two different kinds. 

If listOfAvgFunctions is provided, then the resulting data.frame will have a row for every 
parameter constellation that occurs in the simObject$results. There will be columns for every
parameter used. Also length(listOfAvgFunctions) * length(listOfStatisticFunctions) additional
columns will be created. Every statistic is calculated for every applied procedure and then
all values that belong to a specific parameter constellation are "averaged" by applying all 
function from listOfAvgFunctions. For example, suppose FDP is a function from listOfStatisticFunctions
that calculates the realized false discovery proportion, that is number of true hypotheses that
were rejected divided by the number of all rejected hypotheses. Also suppose mean and sd are 
functions in listOfAvgFunctions, then the mean and the standard deviation of the 
statistic FDP are calculated.  

If listOfAvgFunctions is not provided, then the resulting data.frame will have a row for every
simObject$results. Every parameter will have its own column. Additional columns for every function
in listOfStatisticFunction will be created.}}
\author{MarselScheer}
\arguments{\item{simObject}{An object returned by simulation()}
\item{listOfStatisticFunctions}{List of statistics that shall be calculated for the elements in simObject}
\item{listOfAvgFunctions}{List of functions that will be used to summarize/average the calculated statistics for
all elements in simObject$results with the same parameter constellation. If this is argument is missing no averaging
will be done. Instead the resulting data.frame will keep one row
for every object in simObject$results.}}
\examples{#' # this function generates pValues 
myGen <- function(n, n0) list(procInput=list(pValues = c(runif(n-n0, 0, 0.01), runif(n0))), groundTruth = c(rep(FALSE, times=n-n0), rep(TRUE, times=n0)))

# need some simulation()-Object I can work with 
sim <- simulation(replications = 10, list(funName="myGen", fun=myGen, n=200, n0=c(50,100)), 
list(list(funName="BH", fun=function(pValues, alpha) BH(pValues, alpha, silent=TRUE), alpha=c(0.25, 0.5)),
list(funName="holm", fun=holm, alpha=c(0.25, 0.5),silent=TRUE)))

# Make my own statistic function
NumberOfType1Error <- function(data, result) sum(data$groundTruth * result$rejected)

# Get now for every object in sim$results one row with the number of Type 1 Errors
result.all <- gatherStatistics(sim, list(NumOfType1Err = NumberOfType1Error))

# Average over all sim$results-Objects with common parameters 
result1 <- gatherStatistics(sim, list(NumOfType1Err = NumberOfType1Error), list(MEAN = mean))
print(result1)  
result2 <- gatherStatistics(sim, list(NumOfType1Err = NumberOfType1Error), list(q05 = function(x) quantile(x, probs=0.05), MEAN = mean, q95 = function(x) quantile(x, probs=0.95)))
print(result2)

# create some plots
require(lattice)
histogram(~NumOfType1Err | method*alpha, data = result.all$statisticDF)
barchart(NumOfType1Err.MEAN ~ method | alpha, data = result2$statisticDF)}

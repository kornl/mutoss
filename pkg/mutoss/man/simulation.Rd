\name{simulation}
\alias{simulation}
\title{Simulation studies}
\usage{simulation(replications, DataGen, listOfProcedures)}
\description{This function generates data according to a specified function and parameters
and then applies specified procedures to the generated data. The input and output
is gathered in a MutossSim object. Also the slot "index" of any created MutossSim
is set to a specific number, so that any two MutossSim objects that are based upon the same
input have the same number in the index slot.}
\value{A list of MutossSim ojects. The list can be directly plugged into 
the function gatherStatistics.}
\author{MarselScheer}
\arguments{\item{replications}{The number of replications. This means how many simulation runs will be performed.}
\item{DataGen}{A list that contains the function and parameters for generating data 
which will be analyzed be the procedures in listOfProcedures.}
\item{listOfProcedures}{A list of lists which contains the procedures and parameters
to use for the simulation.}}
\examples{# this function generates pValues 
myGen <- function(n, n0) list(pValues = c(runif(n-n0, 0, 0.01), runif(n0)), groundTruth = c(rep(FALSE, times=n-n0), rep(TRUE, times=n0)))

# Calling 10 times myGen(200, 50)
# Apply then 
# 1. bonferroni with alpha=0.25 to the 1000 lists from myGen
# 2. bonferroni with alpha=0.5 to the 1000 lists from myGen
# 3. holm with alpha=0.25 to the 1000 lists from myGen
# 4. holm with alpha=0.5 to the 1000 lists from myGen
# This yields 40 MutossSim objects
# THEN
# Calling 10 times myGen(200, 100)
# Apply then 
# 1. bonferroni with alpha=0.25 to the 1000 lists from myGen
# 2. bonferroni with alpha=0.5 to the 1000 lists from myGen
# 3. holm with alpha=0.25 to the 1000 lists from myGen
# 4. holm with alpha=0.5 to the 1000 lists from myGen
# This yields 40 MutossSim objects
# Altogether the function simulation returns 80 MutossSim objects.
sim <- simulation(replications = 10, list(funName="myGen", fun=myGen, n=200, n0=c(50,100)), 
list(list(funName="BH", fun=function(pValues, alpha) BH(pValues, alpha, silent=TRUE), alpha=c(0.25, 0.5)),
list(funName="holm", fun=holm, alpha=c(0.25, 0.5),silent=TRUE)))
#
# Just calculating some statistics and making some plots
NumberOfType1Error <- function(MutossSimObject) sum(slot(MutossSimObject, "rejected") * slot(MutossSimObject, "groundTruth"))
result.all <- gatherStatistics(sim, list(NumOfType1Err = NumberOfType1Error))
result <- gatherStatistics(sim, list(NumOfType1Err = NumberOfType1Error), list(median=median, mean=mean, sd=sd))
print(result)
require(lattice)
histogram(~NumOfType1Err | method*alpha, data = result.all$statisticDF)
barchart(NumOfType1Err.median + NumOfType1Err.mean ~ method | alpha, data = result$statisticDF)}

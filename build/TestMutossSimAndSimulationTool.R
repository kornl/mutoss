
FDR <- function(MutossSimObject) 
{
	R <- sum(MutossSimObject@rejected)
	V <- sum(MutossSimObject@rejected * MutossSimObject@groundTruth)
	
	if (R == 0)
		return (0)
	
	return(V/R)	
}

FWER <- function(MutossSimObject)
{
	sum(MutossSimObject@rejected * MutossSimObject@groundTruth) > 0
}

NumberOfType1Error <- function(MutossSimObject)
{
	sum(MutossSimObject@rejected * MutossSimObject@groundTruth)
}

# Just for testing the MutossSim object
# and the simulationTool


require(lattice)

print("START EXAMPLE")
myGen <- function(n, n0) list(pValues=c(runif(n-n0,0,0.01), runif(n0)), groundTruth=c(rep(F, n-n0), rep(T,n0)))
print(system.time(
				test <- simulation(replications = 10,
						list(funName="myGen", fun=myGen, n=200, n0=c(50,100)), 
						list(
								list(
										funName="bonf", 
										fun=bonferroni, 
										alpha=c(0.25, 0.5)
								),
								list(
										funName="holm", 
										fun=holm, 
										alpha=c(0.25, 0.5),
										silent=TRUE
								)				
						)
				)
		)
)


#test.result.all <- gatherStatistics(test, list(FDR = FDR, FWER = FWER, NumOfType1Err = NumberOfType1Error))
#print(test.result.all)
#test.result <- gatherStatistics(test, list(FDR = FDR, FWER = FWER, NumOfType1Err = NumberOfType1Error), mean)
#print(test.result)
print(system.time(
				test.result <- gatherStatistics(
						test, 
						list(FDR = FDR),#, FWER = FWER, NumOfType1Err = NumberOfType1Error), 
						list(
								mittel = mean, 
								std = sd, 
								q05=function(x) quantile(x, probs=0.05), 
								q95=function(x) quantile(x, probs=0.95),
								CIu=function(x) mean(x) - 2*sd(x),
								CIo=function(x) mean(x) + 2*sd(x)
						)
				)
		)
)
print(test.result)
#

#op = par(mfrow=c(1,2))
#barchart(FDR.mittel ~ method | alpha, data = test.result$statisticDF)
#histogram(~FDR | alpha*method, data = test.result.all$statisticDF)
#par(op)

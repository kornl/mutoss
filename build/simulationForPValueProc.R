# Here we make some simulation to see 
# if the SUDProcedures at least hold
# their Error criteria.
# 
# Author: MarselScheer
###############################################################################



FWER 	<- function(mts) sum(mts@rejected * mts@groundTruth) > 0
FDR		<- function(mts) sum(mts@rejected * mts@groundTruth) / max(1, sum(mts@rejected))
nmbRej 	<- function(mts) sum(mts@rejected)
adjPVFeas <- function(mts) 
{
	if (length(mts@adjPValues) == 0)
		return(TRUE)
	
	if ( all(0 <= mts@adjPValues) && all(mts@adjPValues <= 1) )
		return(TRUE)
	
	return(FALSE)
		
}

statistic <-  list(FWER = FWER, FDR = FDR, adjPVFeas = adjPVFeas, nmbRej = nmbRej)

procs <- 		list(
		list(funName="bonf",
				fun = bonferroni,
				alpha = 0.1,
				silent = TRUE),
		list(funName="sidak",
				fun = sidak,
				alpha = 0.1,
				silent = TRUE),
		list(funName="rom",
				fun = rom,
				alpha = 0.1,
				silent = TRUE),
		list(funName="holm",
				fun = holm,
				alpha = 0.1,
				silent = TRUE),
		list(funName="aorc",
				fun = function(pValues, alpha, silent) aorc(pValues, alpha, betaAdjustment = 1.76, silent = silent),
				alpha = 0.1,
				silent = TRUE),
		list(funName="BL",
				fun = BL,
				alpha = 0.1,
				silent = TRUE),
		list(funName="BH",
				fun = BH,
				alpha = 0.1,
				silent = TRUE),
		list(funName="BY",
				fun = BY,
				alpha = 0.1,
				silent = TRUE),								
		list(funName="hochberg",
				fun = hochberg,
				alpha = 0.1,
				silent = TRUE),
		list(funName="hommel",
				fun = hommel,
				alpha = 0.1,
				silent = TRUE),
		list(funName="oracleBH_pi0_1",
				fun = function(pValues, alpha, silent) oracleBH(pValues, alpha, 1, silent),
				alpha = 0.1,
				silent = TRUE),
		list(funName="oracleBH_pi0_0.5",
				fun = function(pValues, alpha, silent) oracleBH(pValues, alpha, 0.5, silent),
				alpha = 0.1,
				silent = TRUE),
		list(funName="oracleBH_pi0_0",
				fun = function(pValues, alpha, silent) oracleBH(pValues, alpha, 0, silent),
				alpha = 0.1,
				silent = TRUE),
		list(funName="adaptiveBH",
				fun = adaptiveBH,
				alpha = 0.1,
				silent = TRUE),
		list(funName="adaptiveSTS",
				fun = adaptiveSTS,
				alpha = 0.1,
				silent = TRUE),
		list(funName="SidakSD",
				fun = SidakSD,
				alpha = 0.1,
				silent = TRUE),
		list(funName="multiple.down",
				fun = multiple.down,
				alpha = 0.1),
		list(funName="two.stage",
				fun = two.stage,
				alpha = 0.1),
		list(funName="indepBR",
				fun = indepBR,
				alpha = 0.1,
				silent = TRUE),
		list(funName="twostageBR",
				fun = twostageBR,
				alpha = 0.1,
				silent = TRUE),
		list(funName="BlaRoq",
				fun = BlaRoq,
				alpha = 0.1,
				silent = TRUE)
)



GlobalNull 	<- function(sampleSize) list(pValues = runif(sampleSize), groundTruth = rep(T, times=sampleSize))
AllWrong	<- function(sampleSize) list(pValues = runif(sampleSize, 0, 0.1), groundTruth = rep(F, times=sampleSize))
HalfWrong	<- function(sampleSize) 
{
	size <- round(sampleSize/2)
	list(
			pValues = c(runif(size, 0, 0.015), runif(size)),
			groundTruth = c(rep(F, size), rep(T, size))
	)
}			

replications <- 10

sim <- simulation(replications,
		list(funName = "GlobalNull",
				fun = GlobalNull,
				sampleSize = 100),
		procs
)
print(length(sim))
result <- gatherStatistics(sim, statistic, list(MEAN = mean, SUM=sum))
print(result$statisticDF)



sim <- simulation(replications,
		list(funName = "HalfWrong",
				fun = HalfWrong,
				sampleSize = 100),
		procs
)
print(length(sim))
result <- gatherStatistics(sim, statistic, list(MEAN = mean, SUM=sum))
print(result$statisticDF)



sim <- simulation(replications,
		list(funName = "AllWrong",
				fun = AllWrong,
				sampleSize = 100),
		procs
)
print(length(sim))
result <- gatherStatistics(sim, statistic, list(MEAN = mean, SUM=sum))
print(result$statisticDF)

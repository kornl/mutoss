# 
# 
# Author: MarselScheer
###############################################################################

rm(list=ls())

pckBasis <- c(
		"helperfunctions.R",
		"SUD.R",
		"SUDProcedures.R"
		
	)

pathBasis <- "~/workspace/mutoss/src/BasicFunctions/"

	
for (p in pckBasis)
	source(paste(pathBasis, p, sep=""))

source("~/workspace/mutoss/src/BigObjectCode/MuTossObject.R")

rm(pckBasis)
rm(pathBasis)


package.skeleton(
		name = "mutoss", 
		list = ls(),
		path = "~/workspace/mutoss/package/")

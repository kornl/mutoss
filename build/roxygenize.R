# Makes the .Rd-Files from RoxygenCodes
# 
# Author: MarselScheer
###############################################################################


#install.packages('/home/MarselScheer/shared/roxygen_0.1-2.tar.gz',rep=NULL)

setwd("~/workspace/mutossRForge/pkg/")

library(roxygen)
roxygenize("mutoss", 
		roxygen.dir = "mutoss",
		copy.package = FALSE,
		unlink.target = FALSE,
		use.Rd2 = TRUE)

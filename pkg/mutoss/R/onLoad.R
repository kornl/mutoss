.onLoad <- function(libname, pkgname) {
	if(!require("multtest", character.only=TRUE)) {
		source("http://bioconductor.org/biocLite.R")
		biocLite("multtest")
		require("multtest")
	}
}  

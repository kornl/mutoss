
requireLibrary <- function(package) {
	if(!require(package, character.only=TRUE)) {
		answer <- readline(paste("Package ",package," is required - should we install it?",sep=""))
		if (substr(answer, 1, 1) %in% c("y","Y")) {
			install.packages(package)
			require(package, character.only=TRUE)
		} else {
			stop(paste("Required package",package,"should not be installed"))
		}
	}
}

startRecording <- function() {
	output <<- "output" # TODO These variablenames are stupid! But there was some bug with hidden ones or non-global ones?
	errorMsg <<- "errorMsg"
	outputCon <<- textConnection(output, open="w")
	errorCon <<- textConnection(errorMsg, open="w")
	sink(outputCon)
	sink(errorCon, type="message")
}

stopRecording <- function() {
	sink()
	sink(type="message")
	close(outputCon)
	close(errorCon)
	return(list(output=output, errorMsg=errorMsg))
}

myContrMat <- function(type,l,df,group) {
	require(multcomp)
	n <- table(df[,group])[as.numeric(factor(l,levels=levels(df[,group])))]
	x <- contrMat(n=n,type=type)
}
mutossGUI.vars <- list()

startRecording <- function() {
	mutossGUI.vars$output <<- "output" # TODO These variablenames are stupid! But there was some bug with hidden ones or non-global ones?
	mutossGUI.vars$errorMsg <<- "errorMsg"
	mutossGUI.vars$outputCon <<- textConnection(mutossGUI.vars$output, open="w")
	mutossGUI.vars$errorCon <<- textConnection(mutossGUI.vars$errorMsg, open="w")
	sink(mutossGUI.vars$outputCon)
	sink(mutossGUI.vars$errorCon, type="message")
}

stopRecording <- function() {
	sink()
	sink(type="message")
	close(mutossGUI.vars$outputCon)
	close(mutossGUI.vars$errorCon)
	return(list(output=mutossGUI.vars$output, errorMsg=mutossGUI.vars$errorMsg))
}

myContrMat <- function(type,l,df,group) {
	require(multcomp)
	n <- table(df[,group])[as.numeric(factor(l,levels=levels(df[,group])))]
	x <- contrMat(n=n,type=type)
}
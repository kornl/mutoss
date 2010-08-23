mutossGUI.vars <- list()

startRecording <- function() {
	mutossGUI.vars$outputCon <- textConnection(".mutossGUIoutput", open="w")
	mutossGUI.vars$errorCon <- textConnection(".mutossGUIerrorMsg", open="w")
	sink(mutossGUI.vars$outputCon)
	sink(mutossGUI.vars$errorCon, type="message")
}

stopRecording <- function() {
	sink()
	sink(type="message")
	try(close(mutossGUI.vars$outputCon), silent = TRUE)
	try(close(mutossGUI.vars$errorCon), silent = TRUE)
	return(list(output=mutossGUI.vars$output, errorMsg=mutossGUI.vars$errorMsg))
}

myContrMat <- function(type,l,df,group) {
	require(multcomp)
	n <- table(df[,group])[as.numeric(factor(l,levels=levels(df[,group])))]
	x <- contrMat(n=n,type=type)
}

myContrMat <- function(type,l,df,group) {
	require(multcomp)
	n <- table(df[,group])[as.numeric(factor(l,levels=levels(df[,group])))]
	x <- contrMat(n=n,type=type)
}

getOutput <- function() {
	return(.mutossGUIoutput)
}

getErrorMsg <- function() {
	return(.mutossGUIerrorMsg)
}
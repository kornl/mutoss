mutossGUI <- function(){
	#gui <- .jnew("mutoss.gui.MuTossGUI")
	.jcall("org/mutoss/gui/MuTossGUI", method="startGUI")
}


myContrMat <- function(type,l,df,group) {
	require(multcomp)
	n <- table(df[,group])[as.numeric(factor(l,levels=levels(df[,group])))]
	x <- contrMat(n=n,type=type) 
}

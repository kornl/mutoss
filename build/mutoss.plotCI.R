############################################
# mutoss.plotCI

#' @export
mutoss.plotCI<-function(mat){
	require(plotrix)
	k<-nrow(mat)
	plotCI(1:k,mat[,1],abs(mat[,2]-mat[,1]),abs(mat[,3]-mat[,1]),lwd=2,col="red",scol="blue",
			main="CI plot",xaxt="n",xlab="Parameters",
			ylab="Values")
	axis(1, at=c(1:k), labels=rownames(mat)) 
}

#require(plotrix)
#y<-runif(10)
#err<-runif(10)
#mat<-cbind(y,err,err*2)
#mutoss.plotCI(mat)

# TODO Please fix!

if (FALSE) {
	ci <- structure(c(0.930000000000001, 1.58200000000000, 0.652000000000001, 
				0.686267599582747, 1.33826759958275, 0.408267599582746, 1.17373240041726, 
				1.82573240041726, 0.895732400417256), .Dim = c(3L, 3L), .Dimnames = list(
				c("versicolor - setosa", "virginica - setosa", "virginica - versicolor"
				), c("Estimate", "lwr", "upr")))

	mutoss.plotCI(ci)
	
}


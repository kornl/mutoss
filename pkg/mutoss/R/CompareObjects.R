# 
# Author: JonathanRosenblatt
###############################################################################

#----------- Validating input of compare procedure------------#


#' Test for compatible classes for comparison
#' 
#' Internal function of muToss package.
#' Takes list of classes and tests if they are all muToss. 
#' @param classes A list of class names. 
#' @return True if all classes are muToss. False otherwise.
#' @author MuToss-Coding Team.

mu.test.class<- function(classes){
	if (any(classes!='Mutoss')) stop('Input is not a "Motoss" class object')
	##TODO: will this cause prolems for inherited class objects?
}

#' Tests that different procedures use the same error types.
#' 
#' Internal muToss function. 
#' @param types Character vector of error types extracted from muToss objects. 
#' @return Returns a notice if error types differ. 
#' @author MuToss-Coding Team.
mu.test.type<- function(types){
	if( any(types != types[1]) ){
		message(' Notice:You are comparing methods for different error types. \n These should not be compared!	\n Output will be generated nevertheless. \n')
	}
}


#' Tests that different procedures used the same error rates.
#' 
#' Internal muToss function. 
#' @param rates Numeric vector error rates extracted fomr muToss objects.
#' @return A notice if error rates differ.
#' @author MuToss-Coding Team.
mu.test.rates<-function(rates){
	if( any(rates != rates[1]) ){
		message(' Notice:You are comparing methods with different error rates. \n These should not be compared!	\n Output will be generated nevertheless. \n')
	}
}

#' Tests if the same pvalues were used by different procedures.
#' 
#' Internal muToss function. 
#' @param pvals Data frame of p-values used by each procedure. 
#' @return Stops if different data was used by different procedures. 
#' @author MuToss-Coding Team
mu.test.same.data<- function(pvals){
	pvals.different<- any( apply(pvals,1, function(x) any(x!=x[1])))
	if(pvals.different) stop('Different data was used for suppied procedures.')		
}


#' Tests if all hypotheses have the same names.
#' 
#' Internal muToss function.
#' @param hyp.names Character vector of hypotheses names.
#' @return Gives a notice when hypotheses names in different procedures are found different. 
#' @author MuToss-Coding Team
mu.test.name<- function(hyp.names){
	names.different<- any( apply(hyp.names,1, function(x) any(x!=x[1])))
	if(names.different) message('Notice: Hypotheses have different names. Can they be compared?')		
}


#-------- Create comparison list for Mutoss objects--------------#

#' Functions for comparing outputs of different procedures.
#' 
#' These functions are used to compare the results of different multiple comparisons procedures stored as Mutoss class objects.
#' \code{compareMutoss} takes as input an arbitrary number of Mutoss objects and arranges them in a simple list objects (non S4).
#' \code{mu.compare.adjuted}, \code{mu.compare.critical} and \code{mu.compare.summary} take the output of the \code{compareMutoss} and plots or summerize the results textually or graphically.
#'
#' @export
#' @usage compareMutoss(...)
#' 		mu.compare.adjusted(comparison.list= compareMutoss(mu.obj.1, mu.obj.2))
#' 		mu.compare.critical(comparison.list= compareMutoss(mu.obj.1, mu.obj.2))
#' 		mu.compare.summary(comparison.list= compareMutoss(mu.obj.1, mu.obj.2))
#' @param ... An arbitrary number of Motoss class objects. 
#' @param comparison.list The output of the \code{compareMutoss} function.
#' @param identify.check Logical parameter specifying if hypotheses should be identified on the output plots.
#' @return \item{compareMutoss}{Returns a list with the following components:\cr
#' 				\bold{types}: Character vector of error types corrsponding to each procedure.\cr
#' 				\bold{rates}: Numeric vector of error rates used for each procedure.\cr
#' 				\bold{pi.nulls}: Numeric vector of estimates of the proportion of true null hypothesis if avilable.\cr
#' 				\bold{raw.pValues}: The raw p-values used for each procedure.\cr
#' 				\bold{adjusted.pvals}: Data frame with columns holding procedure specific adjusted p values.\cr
#' 				\bold{criticalValue}: Data frame with columns holding the critical values corresponding to each procedure and error rate.\cr
#' 				\bold{rejections}: Data frame with columns holding logical vectors of rejected hypotheses (TRUE for rejected).\cr}
#' \item{mu.compare.adjusted}{Creates a plot with the adjusted p-values for each procedure.}
#' \item{mu.compare.critical}{Creates a plot with the critical values for each procedure and error rate.}
#' \item{mu.compare.summary}{Creates a short textual summary for comparing results of different procedures.} 
#' @author Jonathan Rosenblatt 
#' @aliases compareMutoss mu.compare.adjusted mu.compare.critical mu.compare.summary
#' @examples
#' # TODO: EXAMPLE PROBLEMS
#' \dontrun{
#' \dontrun{Creating several Mutoss class objects}
#' mu.test.obj.1 <- mutoss.apply(new(Class="Mutoss", pValues=runif(10)), f=bonferroni, label="Bonferroni Correction", alpha=0.05, silent=T)#' 
#' mu.test.obj.2 <- mutoss.apply(new(Class="Mutoss", pValues=runif(10)), f=holm, label="Holm's step-down-procedure", alpha=0.05, silent=T)#' 
#' mu.test.obj.3 <- mutoss.apply(new(Class="Mutoss", pValues=runif(10)), f=aorc, label="Asymtotically optimal rejection curve", alpha=0.05, startIDX_SUD = 1, silent=T)
#' \dontrun{Trying to coercing a non-Mutoss object}
#' compareMutoss(1)
#' \dontrun{ Coercing several objects into a list}
#' compare.1<- compareMutoss(mu.test.obj.1, mu.test.obj.2)
#' compare.2<- compareMutoss(mu.test.obj.1, mu.test.obj.2, mu.test.obj.3)
#' \dontrun{Plotting the adjusted pvalues. Identification available.}
#' mu.compare.adjusted(compare.1, T)
#' mu.compare.adjusted(compare.2, T)
#' \dontrun{Plotting the critical values. Identification available.}
#' mu.compare.critical(compare.1, T)
#' mu.compare.critical(compare.2, T)
#' \dontrun{Showing a textual sumary}
#' mu.compare.summary(compare.1)
#' mu.compare.summary(compare.2)
#' }

compareMutoss<-function(...){
	objects<-list(...)
	
	classes<- sapply(objects, function(x) class(x) 	)#getting object classes
	mu.test.class(classes) #testing for compatible object classes
	
	types<-	sapply(objects, function(x) x@errorControl@type)#getting error control types
	mu.test.type(types) #testing for compatible error control types
	
	rates<-sapply(objects, function(x) x@errorControl@alpha)#extracting error rates
	
	pi.nulls<- as.numeric(lapply(objects, function(x) x@pi0))#extracting pi0 estimates
	
	hyp.names<- sapply(objects, function(x) x@hypNames )# getting hypothesis names
	mu.test.name(hyp.names)
	hyp.names<- hyp.names[,1]
	
	pvalues<- sapply(objects, function(x) x@pValues )# getting adjusted pvals	
	mu.test.same.data(pvalues)
	
	# Preparing Raw Pvalues
	raw.pvals<- pvalues[,1]
	pval.order<- order(raw.pvals)
	pval.ranks<- rank(raw.pvals)
	raw.pvals.frame<-data.frame(
			pValue=raw.pvals,
			order=pval.order,
			ranks=pval.ranks)
	row.names(raw.pvals.frame)<-hyp.names 
	
	#Preparing adjusted pvalues
	adj.pvals<- lapply(objects, function(x) x@adjPValues )
	method.names<- unlist(lapply(adj.pvals, function(x) attributes(x)[1]))
	adj.pvals.frame<- data.frame(adj.pvals)
	colnames(adj.pvals.frame)<- method.names
	row.names(adj.pvals.frame)<- hyp.names
	
	#Preparing critical values
	critical<- lapply(objects, function(x) x@criticalValues )
	method.names<- unlist(lapply(critical, function(x) attributes(x)[1]))
	critical.frame<- data.frame(critical)
	colnames(critical.frame)<- method.names
	row.names(critical.frame)<- hyp.names	
	
	#Preparing decisions
	rejections<- lapply(objects, function(x) x@rejected )
	method.names<- unlist(lapply(rejections, function(x) attributes(x)[1]))
	rejections.frame<- data.frame(rejections)
	colnames(rejections.frame)<- method.names
	row.names(rejections.frame)<- hyp.names
	
	##TODO: [JR] Add groud truth to comparison method
	
	comparing<- list(
			types=types,
			rates=rates,
			pi.nulls=pi.nulls,
			raw.pValues=raw.pvals.frame,
			adjusted.pvals=adj.pvals.frame,
			criticalValue=critical.frame,
			rejections=rejections.frame
	)
	return(comparing)		
	
}
#For testing purposes
#source('~/workspace/mutoss/src/BasicFunctions/DummyBigObjects.R')
#test<- list(mu.test.obj.1, mu.test.obj.2)

#-------------- Comparison of adjusted p values -----------------#

#' @param comparison.list 
#' @param identify.check 
#' @author JonathanRosenblatt
#' @export
#' @nord
mu.compare.adjusted<- function(comparison.list, identify.check=F){
	adjPValues<- comparison.list[['adjusted.pvals']]
	hyp.num<- nrow(adjPValues)
	method.num<- ncol(adjPValues)
	method.names<- factor(colnames(adjPValues))
	method.index<- as.numeric(method.names)
	raw.pValues<-comparison.list[['raw.pValues']]
	
	pvalue.ranks<-raw.pValues$ranks
	
	method.type<-comparison.list[['types']]
	stacked.adjPValues<- unlist(adjPValues, use.names=F)
	x<- rep(pvalue.ranks, method.num)
	method.labels<- rep(method.names, each=hyp.num)
	hyp.labels<- rep(row.names(adjPValues), method.num)
	point.charachters<- rep(method.index, each=hyp.num) #for plotting purposes only
	
	point.size<- hyp.num^(-0.1)
	plot(stacked.adjPValues~x,
			pch=point.charachters,
			ylim=c(0,1),
			cex=point.size,
			xlab='')
	
	the.title<- paste('Adjusted p-values for ',unique(method.type),' controlling procedures')
	title(the.title)			
	
	par(xpd=T)
	
	legend(x=0, y=-0.15,
			horiz=T,
			legend=method.names,
			pch=method.index,
			cex=method.num ^ (-1/4)	)
	par(xpd=F) #reset par to default value	
	
	if(identify.check) {
		identify(stacked.adjPValues~x, labels=hyp.labels )
	}
	##TODO: [JR] Plotting method using colors?	
}

#For testing purposes
#source('~/workspace/mutoss/src/BasicFunctions/DummyBigObjects.R')

#----------- Comparison of critical vales---------- #

#' @param comparison.list 
#' @param identify.check 
#' @author JonathanRosenblatt
#' @export
#' @nord
mu.compare.critical<- function(comparison.list, identify.check=F){
	method.type<-comparison.list[['types']] #extracting method type
	
	criticalValues<- comparison.list[['criticalValue']]#extracting critical values
	hyp.num<- nrow(criticalValues)
	method.num<- ncol(criticalValues)
	method.names<- factor(colnames(criticalValues))
	method.index<- as.numeric(method.names)
	
	raw.pValues<-comparison.list[['raw.pValues']] #extracting raw palues	
	pvalue.ranks<-raw.pValues$ranks	
	
	stacked.criticalValues<- unlist(criticalValues, use.names=F)
	x<- rep(pvalue.ranks, method.num)
	method.labels<- rep(method.names, each=hyp.num)
	hyp.labels<- rep(row.names(criticalValues), method.num)
	point.charachters<- rep(method.index, each=hyp.num) #for plotting purposes only
	
	point.size<- hyp.num^(-0.1)
	
	plot(stacked.criticalValues~x, 
			pch=point.charachters, 
			ylim=c(0,1),
			xlab='',
			cex=point.size)
	
	the.title<- paste('Critical Values for ',unique(method.type),' controlling procedures')
	title(the.title)			
	
	par(xpd=T)
	legend(
			x=0,
			y=-0.15,
			horiz=T,
			legend=method.names,
			pch=method.index,
			cex=method.num ^ (-1/4)	)
	par(xpd=F) #reset par to default value	
	
	if(identify.check) {
		identify(stacked.criticalValues~x, labels=hyp.labels )
	}
}

#For testing purposes:
#source('~/workspace/mutoss/src/BasicFunctions/DummyBigObjects.R')
#mu.compare.critical(1)
#mu.compare.critical(compare.3, T)

#----- Sumary of comparison-----------#
#' @param comparison.list 
#' @param specific 
#' @author JonathanRosenblatt
#' @export
#' @nord
mu.compare.summary<- function(comparison.list, specific=F){
	method.type<-comparison.list[['types']] #extracting method type
	error.rates<-comparison.list[['rates']] #extracting error rates
	pi.nulls<-comparison.list[['pi.nulls']] #extracting pi0
	rejections<-comparison.list[['rejections']]
	hyp.num<- nrow(rejections)
	method.num<- ncol(rejections)
	method.names<- factor(colnames(rejections))
	method.index<- as.numeric(method.names)
	
	count.rejections<-apply(rejections, 2, sum)
	
	seperate<- rep('|', method.num)	
	summary<-data.frame(
			method.type, seperate, 
			error.rates, seperate, 
			count.rejections, seperate,
			pi.nulls)
	colnames(summary) <-c(
			'Error Type'," ",
			'Error Rate'," ",
			'Rejections Count', " ",
			'pi_0') 
	
	cat('\n Comparing multipe hypothesis procedures.\n',
			hyp.num, 'hypotheses tested.\n\n')
	
	print(summary)	
}

#For testing purposes :
#source('~/workspace/mutoss/src/BasicFunctions/DummyBigObjects.R')
#mu.compare.summary(1)
#mu.compare.summary(compare.3)
# This class is designed for the use in simulation studies.
# 
# Author: MarselScheer
###############################################################################


#' @export
#' @nord
setClass(
		Class = "MutossSim",
		representation    = representation(
				groundTruth 	= "logical", 		# a vector indicating which hypotheses are true resp. false
				parameters 		= "list", 			# a list containing the parameters of the simulation
				idx				= "numeric"		# if two MutossSim objects have the same idx, then they											# are based upon the same data.
		),		
		contains = c("Mutoss")
)



#' Extracts the parameters from the MutossSim objects in listOfObjects and
#' creates a data.frame from this. Every used parameter gets its own column.
#' Every row corresponds to one MutossSim object. If a MutossSim object A does not
#' contain a parameter P that another object B does then the row for object A
#' will have "" in the column for the parameter P.
#' @param listOfObjects A list with MutossSim objects
#' @return A data.frame with rows for every MutossSim object and columns for
#' 		the used parameter.
#' @author MarselScheer
#' @export
gatherParameters <- function(listOfObjects)#, parNames = NULL) 	# argument parNames seems superfluous 
{
	#+++++++++++++++++++	Subfunctions	+++++++++++++++++++++++
	# extract from the MutossSim objects all values of the
	# parameter with the name paraName.
	getParamWithName <- function(listOfObjects, paraName)
	{	
		unlist( 
				lapply(listOfObjects, 
						function(mts)
						{
							val <- mts@parameters[[paraName]]
							if (is.null(val)) 
								return("")
							
							val				
						}
				)
		)	
	}	
	#-------------------	Subfunctions	-----------------------
#	if (is.null(parNames))
	# gathering all parameters used by the MutossSim objects 
	parNames <- unique(unlist(lapply(listOfObjects, function(mts) names(mts@parameters))))
	
	
	# if listOfObjects consists only of one element the return value of gatherParameters
	# will be an data.frame with only one column. But with a additional copy of the
	# first object the return will be a data.frame with one row as it should be.
	if (length(listOfObjects) == 1)
		listOfObjects[[2]] <- listOfObjects[[1]]
	
	# calling data.frame(sapply(parNames, function(pN) getParamWithName(listOfObjects, pN)))
	# is not good, because numeric parameters can be converted to characters and then to factors
	# and it is possible that the original order is lost. For example the order of c(64, 128)
	# will be 128 < 64.
	ret = data.frame(getParamWithName(listOfObjects, parNames[1]))
	for (pN in parNames[2:length(parNames)])
		ret = data.frame(ret, factor(getParamWithName(listOfObjects, pN)))
	

	names(ret) <- parNames
	
	return(ret)
	#return(data.frame(sapply(parNames, function(pN) getParamWithName(listOfObjects, pN))))
	
}


#' This function facilitates gathering statistics from a list of MutossSim objects.
#' 
#' For every MutossSim object in listOfObjects all statistics in listOfStatisticFunctions are calculated. 
#' If in addition listOfAvgFunctions is provided then the statistics of the objects that have the same
#' parameter slot are passed to this functions. The resulting data.frame will then keep only 
#' one row for every parameter constellation.
#' @title Gathering statistics from MutossSim objects
#' @param listOfObjects List of MutossSim objects
#' @param listOfStatisticFunctions List of statistics that shall be calculated for every MutossSim object
#' @param listOfAvgFunctions List of functions that will be used to summarize the calculated statistics of
#' 		all MutossSim objects with the same parameter slot. If this is argument is missing no averaging
#' 		will be done. Instead the resulting data.frame will keep one row
#' 		for every MutossSim object in listOfObjects.
#' @return \item{statisticDF}{A data.frame that can be of two different kinds. 
#' 
#' If listOfAvgFunctions is provided, then the resulting data.frame will have a row for every 
#' parameter constellation that occurs in the MutossSim objects. There will be columns for every
#' parameter used. Also length(listOfAvgFunctions) * length(listOfStatisticFunctions) additional
#' columns will be created. Every statistic is calculated for every MutossSim object and then
#' all values that belong to a specific parameter constellation are "averaged" by applying all 
#' function from listOfAvgFunctions. For example suppose FDP is a function from listOfStatisticFunctions
#' that calculates the realized false discovery proportion, that is number of true hypotheses that
#' were rejected divided by the number of all rejected hypotheses. Also suppose mean and sd are 
#' functions in listOfAvgFunctions, then the mean and the standard deviation of the 
#' statistic FDP are calculated.  
#' 
#' If listOfAvgFunctions is not provided, then the resulting data.frame will have a row for every
#' MutossSim object. Every parameter will have its own column. Additional columns for every function
#' in listOfStatisticFunction will be created additionally.}
#' @author MarselScheer
#' @export
#' @examples
#' #' # this function generates pValues 
#' myGen <- function(n, n0) list(pValues = c(runif(n-n0, 0, 0.01), runif(n0)), groundTruth = c(rep(FALSE, times=n-n0), rep(TRUE, times=n0)))
#' 
#' # need some MutossSim objects I can work with 
#' sim <- simulation(replications = 10, list(funName="myGen", fun=myGen, n=200, n0=c(50,100)), 
#'  	list(list(funName="BH", fun=function(pValues, alpha) BH(pValues, alpha, silent=TRUE), alpha=c(0.25, 0.5)),
#' 			list(funName="holm", fun=holm, alpha=c(0.25, 0.5),silent=TRUE)))
#' 
#' # Make my own statistic function
#' NumberOfType1Error <- function(MutossSimObject) sum(slot(MutossSimObject, "rejected") * slot(MutossSimObject, "groundTruth"))
#' 
#' # Get now for every MutossSim object in sim one row in with the statistic
#' result.all <- gatherStatistics(sim, list(NumOfType1Err = NumberOfType1Error))
#' 
#' # Average over all MutossSim objects with common parameters 
#' result1 <- gatherStatistics(sim, list(NumOfType1Err = NumberOfType1Error), list(MEAN = mean))
#' print(result1)  
#' result2 <- gatherStatistics(sim, list(NumOfType1Err = NumberOfType1Error), list(q05 = function(x) quantile(x, probs=0.05), MEAN = mean, q95 = function(x) quantile(x, probs=0.95)))
#' print(result2)
#' 
#' # do some plots
#' require(lattice)
#' histogram(~NumOfType1Err | method*alpha, data = result.all$statisticDF)
#' barchart(NumOfType1Err.MEAN ~ method | alpha, data = result2$statisticDF) 
gatherStatistics <- function(listOfObjects, listOfStatisticFunctions, listOfAvgFunctions) 
{
	#+++++++++++++++++++++++++++	Subfunctions	++++++++++++++++++++++++++
	# calculates the intersection of all elements given in aList
	listIntersect <- function(aList)
	{
		nn <- length(aList)		
		if(nn == 1)
			return(aList[[1]])
		
		intersect(aList[[1]], listIntersect(aList[-1]))
	}
	# actually the whole work is done by this subfunction.
	gatherStatisticsOneAvgFun <- function(listOfObjects, listOfStatisticFunctions, avgFun, avgFunName = deparse(substitute(avgFun)))
	{
		# extract the parameter constellation of every MutossSim object
		paraNameDF <- gatherParameters(listOfObjects)
		unqParaNameDF <- unique(paraNameDF)
		rownames(unqParaNameDF) <- 1:length(rownames(unqParaNameDF))
		
		# this will be the data.frame containing the parameter constellation and the calculated statistics
		statDF <- data.frame()
		for (i in rownames(unqParaNameDF))
		{
			# search which objects in listOfObjects belong to 
			# parameter configuration in unqParaNameDF[i, ]
			idxs <- listIntersect(
					lapply(names(paraNameDF), 
							function(pN) which(unqParaNameDF[i, pN] == paraNameDF[ , pN])
					)
			)
			
			# applying any given statistic to the objects with the same 
			# parameter constellation
			
			if (missing(avgFun))
			{ # no avgFun, thus the resulting data.frame will have one row for every MutossSim object
				tmp <- sapply(listOfStatisticFunctions,			
						function(fun) sapply(idxs, function(idx) fun(listOfObjects[[idx]]))
				)
				statDF <- rbind(statDF, cbind(paraNameDF[idxs,], tmp))
			}else
			{ 	# avgFun supplied, thus the the resulting data.frame will have only one row for
				# every parameter constellation
				statDF <- rbind(statDF, 
						sapply(listOfStatisticFunctions,
								function(fun) avgFun(sapply(idxs, function(idx) fun(listOfObjects[[idx]])))			
						)
				)			
			}
		}
		
		
		if (missing(avgFun))
		{
			# number the rows consecutively
			rownames(statDF) <- 1:length(rownames(statDF))
		}else
		{
			# label the columns of the resulting data.frame
			names(statDF) <- paste(names(listOfStatisticFunctions), avgFunName, sep=".")
			statDF <- cbind(unqParaNameDF, statDF)		
		}
		
		if (missing(avgFun))
			return(
					list(
							statisticDF = statDF, 
							name.parameters = names(paraNameDF), 
							name.statistics = names(listOfStatisticFunctions), 
							name.avgFun = ""
					)
			)
		
		list(
				statisticDF = statDF, 
				name.parameters = names(paraNameDF), 
				name.statistics = paste(names(listOfStatisticFunctions), avgFunName, sep="."), 
				name.avgFun = avgFunName
		)
	}
	#---------------------------	Subfunctions	--------------------------	
	
	# if no average function is given 
	# the resulting data.frame will have one row 
	# for every MutossSim object in listOfObjects
	if (missing(listOfAvgFunctions))
		return(gatherStatisticsOneAvgFun(listOfObjects, listOfStatisticFunctions))
	
	# the average function is a list, pass this directly to 
	# gatherStatisticsOneAvgFun
	if (is.function(listOfAvgFunctions))
	{		
		return(gatherStatisticsOneAvgFun(
						listOfObjects, 
						listOfStatisticFunctions, 
						listOfAvgFunctions, 
						deparse(substitute(listOfAvgFunctions))
				)
		)
	}
	
	# call gatherStatisticsOneAvgFun for every function in 
	# listOfAvgFunctions 
	if (length(listOfAvgFunctions) > 0)
	{
		if (is.null(names(listOfAvgFunctions)))
			stop("The functions in listOfAvgFunctions must have a name!")
		
		tmp <- list()
		# cnt is needed to determine the name of "fun"
		cnt <- 0
		for (fun in listOfAvgFunctions)
		{
			cnt <- cnt + 1
			tmp[[cnt]] <- gatherStatisticsOneAvgFun(
					listOfObjects,
					listOfStatisticFunctions,
					fun,
					names(listOfAvgFunctions)[cnt]
			)
		}
		
		
		# We have gathered many statistics, now join the information 
		ret <- tmp[[1]]
		ret$statisticDF <- ret$statisticDF[ret$name.parameters]
		
		
		for (i in seq(along.with = listOfAvgFunctions))
		{
			ret$statisticDF <- cbind(ret$statisticDF, tmp[[i]]$statisticDF[tmp[[i]]$name.statistics])
			ret$name.statistics <- c(ret$name.statistics, tmp[[i]]$name.statistics)
			ret$name.avgFun <- c(ret$name.avgFun, tmp[[i]]$name.avgFun)
		}
		
		ret$name.statistics <- unique(ret$name.statistics)
		ret$name.avgFun <- unique(ret$name.avgFun)
		
		return(ret)
	}	
}

# TODO: integrate keepSlots 
#' This function generates data according to a specified function and parameters
#' and then applies specified procedures to the generated data. The input and output
#' is gathered in a MutossSim object. Also the slot "index" of any created MutossSim
#' is set to a specific number, so that any two MutossSim objects that are based upon the same
#' input have the same number in the index slot.
#'  
#' 
#' @title Simulation studies
#' @param replications The number of replications. This means how many simulation runs will be performed.  
#' @param DataGen A list that contains the function and parameters for generating data 
#' 		which will be analyzed be the procedures in listOfProcedures. 
#' @param listOfProcedures A list of lists which contains the procedures and parameters
#' 		to use for the simulation. 
#' @return A list of MutossSim ojects. The list can be directly plugged into 
#' 		the function gatherStatistics.
#' @author MarselScheer
#' @export
#' @examples
#' # this function generates pValues 
#' myGen <- function(n, n0) list(pValues = c(runif(n-n0, 0, 0.01), runif(n0)), groundTruth = c(rep(FALSE, times=n-n0), rep(TRUE, times=n0)))
#' 
#' # Calling 10 times myGen(200, 50)
#' # Apply then 
#' # 1. bonferroni with alpha=0.25 to the 1000 lists from myGen
#' # 2. bonferroni with alpha=0.5 to the 1000 lists from myGen
#' # 3. holm with alpha=0.25 to the 1000 lists from myGen
#' # 4. holm with alpha=0.5 to the 1000 lists from myGen
#' # This yields 40 MutossSim objects
#' # THEN
#' # Calling 10 times myGen(200, 100)
#' # Apply then 
#' # 1. bonferroni with alpha=0.25 to the 1000 lists from myGen
#' # 2. bonferroni with alpha=0.5 to the 1000 lists from myGen
#' # 3. holm with alpha=0.25 to the 1000 lists from myGen
#' # 4. holm with alpha=0.5 to the 1000 lists from myGen
#' # This yields 40 MutossSim objects
#' # Altogether the function simulation returns 80 MutossSim objects.
#' sim <- simulation(replications = 10, list(funName="myGen", fun=myGen, n=200, n0=c(50,100)), 
#'  	list(list(funName="BH", fun=function(pValues, alpha) BH(pValues, alpha, silent=TRUE), alpha=c(0.25, 0.5)),
#' 			list(funName="holm", fun=holm, alpha=c(0.25, 0.5),silent=TRUE)))
#' #
#' # Just calculating some statistics and making some plots
#' NumberOfType1Error <- function(MutossSimObject) sum(slot(MutossSimObject, "rejected") * slot(MutossSimObject, "groundTruth"))
#' result.all <- gatherStatistics(sim, list(NumOfType1Err = NumberOfType1Error))
#' result <- gatherStatistics(sim, list(NumOfType1Err = NumberOfType1Error), list(median=median, mean=mean, sd=sd))
#' print(result)
#' require(lattice)
#' histogram(~NumOfType1Err | method*alpha, data = result.all$statisticDF)
#' barchart(NumOfType1Err.median + NumOfType1Err.mean ~ method | alpha, data = result$statisticDF)
simulation <- function(replications, DataGen, listOfProcedures) 
{	
	# TODO: MS !! the same parameterNames for DataGen and listOfProcedures will cause problems
	# TODO: MS print progress of the simulation on the console!
	#+++++++++++++++++++++++	Subfunctions	+++++++++++++++++++
	
	# Generates a list of lists. Every of these lists, denoted for now
	# by L, can be evaluated as a function call by eval(as.call(L))
	# listFunAndParameter MUST have the following form:
	# TODO: MS  Comments not well understandable [TD] !
	# list(
	#		funName = "UnifRandom",					# Description/Label of the function to be used
	#		fun		= runif,					# A real function, not only the name
	#		n		= 1						# "n" is integer   
	# )
	
	# Example for the argument:
	# Arguments of runif are: n, min, max.
	# listFunAndParameter = list(funName="UnifRandomVariable", fun=runif, n=2, min=c(1:2), max=c(1.1, 2.1))
	# sapply(generateFunctionStack(listFunAndParameter), function(fc) eval(as.call(fc)))
	# is same as
	# runif(2, 1, 1.1); runif(2, 2, 1.1); runif(2, 1, 2.1); runif(2, 2, 2.1)
	# Of course the second and third call do not make sense.
	generateFunctionStack <- function(listFunAndParameter) 
	{
		# listFunAndParameter[[3]] is the first real parameter. The first 2 are the function to
		# to be called and a description.
		outerPar <- 1:length(listFunAndParameter[[3]])
		
		# Actually I want to build the outerproduct of the parameters,
		# but instead of this I use index numbers indicating the position
		# of the used parameter. If  n=c("a", "b"), n0=c(1:5), alpha=c(0.1, 0.2)
		# then 2, 3, 2 stands for n="b", n0=3, alpha=0.2
		for(par in listFunAndParameter[-3:-1])
			outerPar <- outer(outerPar, 1:length(par), paste)
		
		# special case of only ONE parameter
		if (length(listFunAndParameter) == 3)
			outerPar <- outer(outerPar, "", paste)		
		
		# build now for every parameter constellation
		# a list that can be casted into a function call. 
		fcStack <- list()
		for (parIDX in outerPar)
		{				
			idx <- as.numeric(unlist(strsplit(parIDX, " ")))
			
			parameter <- list()
			for(i in 1:length(listFunAndParameter[-2:-1]))
				# listFunAndParameter[-2:-1][[i]] is the i-th parameter in the list.
				# from the 1st parameter we want the idx[1]-th entry from the 2nd parameter
				# we want the idx[2]-th entry and so on.
				parameter <- c(parameter, listFunAndParameter[-2:-1][[i]][idx[i]])
			
			stackPosName <- paste(listFunAndParameter$funName, parIDX)
			fcStack[[stackPosName]] <- c(listFunAndParameter$fun, parameter)
			names(fcStack[[stackPosName]]) <- names(listFunAndParameter[-1])
		}
		
		return(fcStack)
	}
	#-----------------------	Subfunctions	-------------------
	
	# generating all data generating functions
	dataGenStack 	<- generateFunctionStack(DataGen)
	AllPossibleSlotNames <- slotNames(new("MutossSim"))
	# a bunch of stacks full of procedure functions
	# for example for every method (bonferroni and holm) there
	# is a stack for bonferroni with the different parameter configurations
	# for bonferroni and a stack for holm with the different parameter configurations for holm
	procedureStacks	<- lapply(listOfProcedures, function(procs) generateFunctionStack(procs))
	names(procedureStacks) 	<- sapply(listOfProcedures, function(procs) procs$funName)
	
	ret = list()
	
	# cnt is used as an identifier. So every MutossSimObject with
	# the same @idx are based on the same @pValues or @data.
	cnt <- 0
	for( dataGenCall in dataGenStack )
	{	
		# This is probably the right place for gridComputation
		
		# generates a bunch of MutossSimObjects. It calls ONE time
		# dataGenCall. Every procedure in procedureStacks is applied
		# on this one "dataSet". The input and output for every procedure
		# is gathered in a MutossSimObject.
		genMutossSimObjects <- function(dummy) 
		{
			# generating data
			data <- eval(as.call(dataGenCall))
			
			# cnt is a global variable that has to be increased
			# each time a new "dataSet" is generated.
			assign("cnt", cnt + 1, envir = sys.frame(-3))
			
			# retVal will carry the generated MutossSimObjects
			retVal <- list()
			for (pS in seq(along.with=procedureStacks))
			{
				# procedureStacks consists of stacks, go through one by one
				procStack <- procedureStacks[[pS]]
				for (proc in procStack)
				{
					
					m <- new("MutossSim")
					m@idx <- get("cnt", envir  = sys.frame(-3))
					# writing the "input" given from the data generating function into
					# the MutossSimObject
					for (slotName in names(data))
						slot(m, slotName) <- data[[slotName]]
					
					
					# saving the parameter constellation of the used by the data generating function
					paramDataGen			<- c(DataGen$funName, dataGenCall[-1])
					names(paramDataGen)[1]	<- "funName"
					# saving the parameter constallation of the used procedure
					paramProc				<- c(names(procedureStacks)[pS], proc[-1])
					names(paramProc)[1]		<- "method"
					
					# write the parameter constallation of the data generating function and the procedure 
					# into the MutossSimObject
					m@parameters			<- c(paramDataGen, paramProc)
					
					if (is.element("pValues", names(data)))
						# telling the procedure which pValues should be used.
						proc$pValues <- data$pValues
					else
						stop("simulation(): To implement")
					
					# calling the procedure
					procOutput 	<- eval(as.call(proc))
					
					# writing the output of the procedure into the MutossSimObject
					for(slotName in names(procOutput))
						if (is.element(slotName, AllPossibleSlotNames))
							slot(m, slotName) <- procOutput[[slotName]]
					
					# append the new MutossSimObject
					retVal <- c(retVal, m)
				}
			}
			return(retVal)
		}
		ret <- c(ret, unlist(lapply(1:replications, genMutossSimObjects)))		
	}
	
	return(ret)
}

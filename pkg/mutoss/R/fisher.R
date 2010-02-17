#############
# Fisher 23 #
#############


#' Fisher-type (2x3) table as model for marginal hypotheses testing problems 
#' @return A list containing the model description
#' @author ThorstenDickhaus
#' @export
fisher23.model <- function() {
	return(list(model=list(typ="Fisher 2-by-3")))
}

#' Fisher-type (2x3) table as model for marginal hypotheses testing problems 
#' @return A list containing the model description
#' @author ThorstenDickhaus
#' @export
mutoss.fisher23.model <- function() { return(new(Class="MutossMethod",
					label="Fisher's exact test in (2x3) tables",
					callFunction="fisher23.model",
					output=c("model"),
					info="<h2></h2>
							<p>(Marginal) Fisher's exact test in (2x3) tables</p> 
							<p></p>
							<h3>Reference:</h3>
							<ul>
							<li>Fisher, R. A. (1922). \"<i>On the interpretation of Chi^2 from contingency tables, and the calculation of P.</i>\" Journal of the Royal Statistical Society, 85 (1):87-94.</li>
							</ul>",
					parameters=list(
					)
			)) }


#' Fisher-type (2x3) table association analysis for calculating all marginal p-values  
#' @param data A tensor of dimension (2x3xm) where m is the number of endpoints (genes, etc.)
#' @param model A model object indicating that this type of analysis shall be performed
#' @return A list containing the m marginal p-values
#' @author ThorstenDickhaus
#' @export
fisher23.marginal <- function(data, model) {
	#m <- dim(data)[3]
	#result <- vector(mode="numeric",length=m)
	result <- apply(data, 3, function(x) {fisher23_fast(x,2.0e-16)$rand_p} )
	return(list(pValues=result))
}


#' Fisher-type (2x3) table association analysis for calculating one (marginal) p-value
#' @param obs The observed (2x3) table 
#' @param epsilon A threshold for comparing real numbers to zero
#' @return A list containing:
#' 
#'	\item{nonrand_p}{The non-randomized (conservative) p-value}
#' 
#'	\item{rand_p}{The randomized (non-conservative) p-value}
#' 
#'	\item{prob_table}{The conditional probability of the observed table (given the merginals)}
#' @author ThorstenDickhaus
#' @export
fisher23_fast <- function(obs, epsilon){
# obs = observations = a 2x3 table
	
#build marginals = (n1., n2., n.1, n.2, n.3)
	marginals <- c(sum(obs[1, ]), sum(obs[2, ]), sum(obs[, 1]), sum(obs[, 2]), sum(obs[, 3]))
	
	n <- sum(marginals) / 2
	
	x <- array(data=rep.int(0.0, 2*3*max(marginals)*max(marginals)), c(2, 3, max(marginals)*max(marginals)))
	
#build log nominator statistic
	log_nom <- sum(log(gamma(marginals+1))) - log(gamma(n+1))
	
#build log denominator statistic
	log_denom <- sum(log(gamma(obs+1)))
	
	
#compute probability of observed table
	prob_table <- exp(log_nom - log_denom)
	
	
	nonrand_p <- 0.0
	rand_count <- 0
	
	dim1 <- min(marginals[1], marginals[3])
	
#traverse all possible tables with given marginals
	counter <- 0
	
	for (k in 0:dim1)
	{
		for (l in max(0,marginals[1]-marginals[5]-k):min(marginals[1]-k, marginals[4]))
		{
			counter <- counter+1
			x[1, 1, counter] <- k
			x[1, 2, counter] <- l
			x[1, 3, counter] <- marginals[1] - x[1, 1, counter] - x[1, 2, counter]
			x[2, 1, counter] <- marginals[3] - x[1, 1, counter]
			x[2, 2, counter] <- marginals[4] - x[1, 2, counter]
			x[2, 3, counter] <- marginals[5] - x[1, 3, counter]
		}
	}
	
	log_denom_iter <- rep.int(0.0, times=counter)
	for (k in 1:counter)
	{
		log_denom_iter[k] <- sum(log(gamma(x[, , k]+1)))
	}
	prob_lauf <- exp(log_nom - log_denom_iter)
	nonrand_p <- sum(prob_lauf[prob_lauf <= prob_table])        
	rand_count <- sum(abs(exp(prob_lauf) - exp(prob_table)) < epsilon)        
	
	u <- runif(1)
	rand_p <- max(0.0, nonrand_p - u*rand_count*prob_table)
	
	return(list(nonrand_p=nonrand_p, rand_p=rand_p, prob_table=prob_table))
}


#############
# Fisher 22 #
#############


#' Fisher (2x2) table as model for marginal hypotheses testing problems
#' @return A list containing the model description 
#' @author ThorstenDickhaus
#' @export
fisher22.model <- function() {
	return(list(model=list(typ="Fisher 2-by-2")))
}


#' Fisher (2x2) table as model for marginal hypotheses testing problems 
#' @return A list containing the model description
#' @author ThorstenDickhaus
#' @export
mutoss.fisher22.model <- function() { return(new(Class="MutossMethod",
					label="Fisher's exact test in (2x2) tables",
					callFunction="fisher22.model",
					output=c("model"),
					info="<h2></h2>
							<p>(Marginal) Fisher's exact test in (2x2) tables</p> 
							<p></p>
							<h3>Reference:</h3>
							<ul>
							<li>Fisher, R. A. (1922). \"<i>On the interpretation of Chi^2 from contingency tables, and the calculation of P.</i>\" Journal of the Royal Statistical Society, 85 (1):87-94.</li>
							</ul>",
					parameters=list(
					)
			)) }


#' Fisher (2x2) table association analysis for calculating all marginal p-values  
#' @param data A tensor of dimension (2x2xm) where m is the number of endpoints (genes, etc.)
#' @param model A model object indicating that this type of analysis shall be performed
#' @return A list containing the m marginal p-values
#' @author ThorstenDickhaus
#' @export
fisher22.marginal <- function(data, model) {
	#m <- dim(data)[3]
	#result <- vector(mode="numeric",length=m)
	result <- apply(data, 3, function(x) {fisher22_fast(x,2.0e-16)$rand_p} )
	return(list(pValues=result))
}


#' Fisher (2x2) table association analysis for calculating one (marginal) p-value
#' @param obs The observed (2x2) table 
#' @param epsilon A threshold for comparing real numbers to zero
#' @return A list containing:
#' 
#'	\item{nonrand_p}{The non-randomized (conservative) p-value}
#' 
#'	\item{rand_p}{The randomized (non-conservative) p-value}
#' 
#'	\item{prob_table}{The conditional probability of the observed table (given the merginals)}
#' @author ThorstenDickhaus
#' @export
fisher22_fast <- function(obs, epsilon){
# obs = observations = a 2x2 table
	
#build marginals = (n1., n2., n.1, n.2)
	marginals <- c(sum(obs[1, ]), sum(obs[2, ]), sum(obs[, 1]), sum(obs[, 2]))
	
	n <- sum(obs)
	
	x <- array(data=rep.int(0.0, 2*2*max(marginals)*max(marginals)), c(2, 2, max(marginals)*max(marginals)))
	
#build log nominator statistic
	log_nom <- sum(log(gamma(marginals+1))) - log(gamma(n+1))
	
#build log denominator statistic
	log_denom <- sum(log(gamma(obs+1)))
	
	
#compute probability of observed table
	prob_table <- exp(log_nom - log_denom)
	
	
	nonrand_p <- 0.0
	rand_count <- 0
	
	dim1 <- min(marginals[1], marginals[3])
	
#traverse all possible tables with given marginals
	counter <- 0
	
	for (k in (marginals[1]-marginals[4]):dim1)
	{
		counter <- counter+1
		x[1, 1, counter] <- k
		x[1, 2, counter] <- marginals[1] - k
		x[2, 1, counter] <- marginals[3] - k
		x[2, 2, counter] <- marginals[2] - x[2, 1, counter]
	}
	
	log_denom_iter <- rep.int(0.0, times=counter)
	for (k in 1:counter)
	{
		log_denom_iter[k] <- sum(log(gamma(x[, , k]+1)))
	}
	prob_lauf <- exp(log_nom - log_denom_iter)
	nonrand_p <- sum(prob_lauf[prob_lauf <= prob_table])        
	rand_count <- sum(abs(exp(prob_lauf) - exp(prob_table)) < epsilon)        
	
	u <- runif(1)
	rand_p <- max(0.0, nonrand_p - u*rand_count*prob_table)
	
	return(list(nonrand_p=nonrand_p, rand_p=rand_p, prob_table=prob_table))
}


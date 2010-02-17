# TODO: Add comment
# 
# Author: JonathanRosenblatt
###############################################################################


## Recalling Efron's fdr procedure ##
install.packages('locfdr')
help('ocfdr')


## Re-writing Efron's fdr ##

t_statistics.1<- sapply(data_list.1, my_ttest) #returns t statistic of each hypothesis
t_statistics.2<- sapply(data_list.2, my_lm) # returns t statistic of each contrast

statistics.1 <- qnorm(pt(t_statistics.1, df_vector)) #convert t to z statistic
statistics.2 <- qnorm(pt(t_statistics.2, df_vector)) #convert t to z statistic

my_fdr <- function(statistics, cutoff, type, p0, ...){
	
	f_0 <- switch(type, 
			theoretical = theoretic_f_0(statistics, tail),
			ML = ml_f_0(statistics, tail,),
			CM = cm_f_0(statistics, tail)
	) #Bad for user integration :-(. Better get an f_0 vector.
	
	f <- estimate_f(statistics, method, ...)
	
	if(p0 == NULL) {
		po <- estimate_p0(statistics, method)
	}
	
	fdr <- f_0 * p0 / f
	
	return(list(
					desicion = fdr < cutoff,
					marginal_density = f,
					statistics = statistics,
					method= ...
			))	
}

test_result.1 <- my_fdr(statistics.1, 0.8, 'ML', p0=NULL)
test_result.2 <- my_fdr(statistics.2, 0.8, 'theoretical', p0=NULL)

confidence_correction<- function(my_fdr_output, method){
		alpha<- switch(method, 
			method_1 = foo1(my_fdr_output),
			method_2 = foo2(my_fdr_output)
	)
}

alpha.1<- confidence_correction(test_result.1, 'Simultanous')
alpha.2<- confidence_correction(test_result.2, 'Simultanous')

cis.1 <- sapply(data_list.1, my_ttest, alpha.1 )
cis.2 <- sapply(data_list.2, my_lm, alpha.2 )


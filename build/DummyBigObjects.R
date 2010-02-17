# 
# Author: JonathanRosenblatt
###############################################################################

n<- 100
alpha<- 0.05
pvals<- runif(n) ^ 3
hyp.names<- paste('hyp', 1:n, sep='')

#---- Bonferonni (dummy) mutoss class object --------# 

name<-'Bonferoni'
bonf.adjusted<- pmin( pvals * n, 1)
attr(bonf.adjusted, 'method.name') <- name
bonf.critic<- rep(alpha / n, n)
attr(bonf.critic, 'method.name') <- name
bonf.reject<- as.logical(pvals <= bonf.critic )
attr(bonf.reject, 'method.name') <- name

mu.err.bonf.1<- new(Class='ErrorControl',
		type     = "FWER",
		alpha    = alpha,   
		k        = numeric(0),  
		q        = numeric(0)
)


mu.test.obj.1<- new(Class='Mutoss',
		description     = 'Test object #1',
		hypNames        = hyp.names,
		pValues         = pvals, 
		adjPValues      = bonf.adjusted,   
		errorControl    = mu.err.bonf.1,		
		criticalValues	= bonf.critic,
		rejected        = bonf.reject
)


#------ Sidack dummy object ---------#
name<- 'Sidack'
sidack.adjusted<- 1 - (1 - pvals) ^ n
attr(sidack.adjusted, 'method.name')<- name
sidack.critic<- rep( 1 - (1 - alpha)^(1 / n), n)
attr(sidack.critic, 'method.name') <- name
sidack.reject<- as.logical(pvals <= sidack.critic )
attr(sidack.reject, 'method.name')<-name 

mu.err.sidack.1<-new(Class='ErrorControl',
		type     = "FWER",
		alpha    = alpha,   
		k        = numeric(0),  
		q        = numeric(0)
)

mu.test.obj.2<- new(Class='Mutoss',
		description     = 'Test object #2',
		hypNames        = hyp.names,
		pValues         = pvals, 
		adjPValues      = sidack.adjusted ,   
		errorControl    = mu.err.sidack.1,	
		criticalValues	= sidack.critic,
		rejected        = sidack.reject
)

#------ BH dummy object ---------#

name<- 'BH'
bh.adjusted<- p.adjust(pvals,'BH')
attr(bh.adjusted, 'method.name')<-name 
bh.critic<- alpha / n * rank(pvals)
attr(bh.critic, 'method.name')<- name
bh.reject<- as.logical(bh.adjusted <= alpha)
attr(bh.reject, 'method.name')<- name

mu.err.bh.1<-new(Class='ErrorControl',
		type     = "FDR",
		alpha    = alpha,   
		k        = numeric(0),  
		q        = numeric(0)
)

mu.test.obj.3<- new(Class='Mutoss',
		description     = 'Test object #3',
		hypNames        = hyp.names,
		pValues         = pvals, 
		adjPValues      = bh.adjusted,   
		errorControl    = mu.err.bh.1,
		criticalValues	= bh.critic,
		rejected        = bh.reject
)

test<- list(mu.test.obj.1, mu.test.obj.2)

#------- Adaptive BH dummy object-------#
name<- 'Adaptive BH'
library(mcp.project)
adaptive.bh.adjusted<- fdr(pvals,q=0.1,'BH Adaptive')$Pvals[['adjusted.pvals']]		
attr(adaptive.bh.adjusted, 'method.name')<-name 
adaptive.bh.critic<- fdr(pvals,q=alpha,'BH Adaptive')$Pvals[['criticals']]
attr(adaptive.bh.critic, 'method.name')<- name
adaptive.bh.reject<- as.logical(adaptive.bh.adjusted <= alpha)
attr(adaptive.bh.reject, 'method.name')<- name

mu.err.abh.1<-new(Class='ErrorControl',
		type     = "FDR",
		alpha    = alpha,   
		k        = numeric(0),  
		q        = numeric(0)
)

mu.test.obj.4<- new(Class='Mutoss',
		description     = 'Test object #4',
		hypNames        = hyp.names,
		pValues         = pvals, 
		adjPValues      = adaptive.bh.adjusted,   
		errorControl    = mu.err.abh.1,
		criticalValues	= adaptive.bh.critic,
		rejected        = adaptive.bh.reject,
		pi0             = bh.m0.estimate(sort(pvals),length(pvals)) / length(pvals)
)

#------- Tukey CI dummy object-------#
library(multcomp)
lmod <- lm(Fertility ~ ., data = swiss)
K <- diag(length(coef(lmod)))[-1,]
rownames(K) <- names(coef(lmod))[-1]
tukey.temp<-confint(glht(lmod, linfct = K))

amod <- aov(breaks ~ tension, data = warpbreaks)
glht(amod, linfct = mcp(tension = "Tukey"))





name<- 'Tukey HSD'
tukey.adjusted<- 	
attr(tukey.adjusted, 'method.name')<-name 
tukey.critic<- 
attr(tukey.critic, 'method.name')<- name
tukey.reject<- as.logical(tukey.adjusted <= alpha)
attr(tukey.reject, 'method.name')<- name

mu.err.tukey.1<-new(Class='ErrorControl',
		type     = "FWER",
		alpha    = alpha,   
		k        = numeric(0),  
		q        = numeric(0)
)

mu.test.obj.4<- new(Class='Mutoss',
		description     = 'Test object #5',
		hypNames        = hyp.names,
		pValues         = pvals, 
		adjPValues      = tukey.adjusted,   
		errorControl    = mu.err.tukey.1,
		criticalValues	= tukey.critic,
		rejected        = tukey.reject,
		model			= ,           
		statistic       = ,
		confIntervals   = tukey.temp$confint
)


#test<- list(mu.test.obj.1, mu.test.obj.2)

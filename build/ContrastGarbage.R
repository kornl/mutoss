library(multcomp)
data <- data.frame(X=rnorm(100), Y=rep(LETTERS[1:5],20), Z=rep(LETTERS[1:5], each=20))
ResponseName = "X"
CovarName = "Y"
GroupName = "Z"
LevelNames = LETTERS[1:5]
conf.level=0.95
cmethod="Tukey"
alternative="two.sided"
margin=0
nointeraction=TRUE
withinteraction=TRUE
modelcomparison=TRUE

covar_independent_factor.calc <- function(data, ResponseName, CovarName, GroupName, LevelNames, conf.level=0.95, cmethod, alternative="two.sided", margin=0,
		nointeraction=TRUE, withinteraction=TRUE, modelcomparison=TRUE){
	require(multcomp)
	if (is.null(GroupName)){
		subdat <- data.frame(data[,c(ResponseName, CovarName)])
		names(subdat) <- c(ResponseName, CovarName)
	} else {
		subdat1 <- data[,c(ResponseName, CovarName, GroupName)]
		subdat2 <- subdat1[subdat1[,GroupName] %in% LevelNames,]
		subdat <- na.omit(subdat2)
		subdat[,GroupName] <- factor(subdat[,GroupName], levels=LevelNames)
	}
	subdat <- subdat[order(subdat[,CovarName]),]
	subdat$Covarf <- as.factor(subdat[,CovarName])
	
	
	
	
	## No Interaction
	if (nointeraction){
		form1 <- as.formula(paste(ResponseName, " ~ ", GroupName, " + Covarf", sep=""))
		fit1 <- lm(form1, data=subdat)
		
		ANOVA1 <- as.data.frame(anova(fit1))
		rownames(ANOVA1) <- c(GroupName, CovarName, "Residuals")
		
		n <- tapply(subdat[,ResponseName], subdat[,GroupName], length)
		CMAT <- contrMat(n=n, type=cmethod)
		comps <- list(CMAT)
		names(comps) <- GroupName
		mymcp <- do.call("mcp", comps)
		glhtobj1 <- glht(fit1, linfct=mymcp, alternative=alternative, rhs=margin)
		PVs1 <- summary(glhtobj1)
		CI1 <- confint(glhtobj1, level=conf.level)
		PVals1 <- data.frame(Estimate=PVs1$test$coefficients,sigma=PVs1$test$sigma,tstat=PVs1$test$tstat,adj.pvalue=PVs1$test$pvalue)
		SCIs1 <- cbind(rbind(PVals1), rbind(CI1$confint[,c(2,3)]))
		attr(SCIs1, which="caption") <-  paste("Simultaneous ", round(conf.level*100), "%-confidence intervals and adjusted p-values for differences for ",attr(CMAT, name="type") ,"-contrasts.", sep="")
	} else {
		fit1 <- NULL
		ANOVA1 <- NULL
		SCIs1 <- NULL
	}
	
	
	## Interaction
	if (withinteraction){
		form2 <- as.formula(paste(ResponseName, " ~ ", GroupName, " + Covarf + ", GroupName, ":Covarf", sep=""))
		fit2 <- lm(form2, data=subdat)
		
		ANOVA2 <- as.data.frame(anova(fit2))
		rownames(ANOVA2) <- c(GroupName, CovarName, paste(GroupName, CovarName, sep=":"), "Residuals")
		
		cmdat <- subdat
		nc <- length(levels(subdat$Covarf))
		nt <- length(levels(subdat[,GroupName]))
		lev <- paste(rep(levels(subdat[,GroupName]), each=nc),rep(levels(subdat$Covarf), times=nt), sep=":")
		cmdat$cvgroup <- factor(paste(subdat[,GroupName],subdat[,CovarName], sep=":"), levels=lev)
		formcm <- as.formula(paste(ResponseName, " ~ cvgroup -1", sep=""))
		fitcm <- lm(formcm, data=cmdat)
		cmmat <- kronecker(CMAT, diag(length(levels(subdat$Covarf))))
		comps <- list(cmmat)
		names(comps) <- "cvgroup"
		mymcp <- do.call("mcp", comps)
		glhtobj2 <- glht(fitcm, linfct=mymcp, alternative=alternative, rhs=margin)
		PVs2 <- summary(glhtobj2)
		CI2 <- confint(glhtobj2, level=conf.level)
		PVals2 <- data.frame(Estimate=PVs2$test$coefficients,sigma=PVs2$test$sigma,tstat=PVs2$test$tstat,adj.pvalue=PVs2$test$pvalue)
		SCIs2 <- cbind(rbind(PVals2), rbind(CI2$confint[,c(2,3)]))
		rownames(SCIs2) <- paste(rep(rownames(CMAT), each=length(levels(subdat$Covarf))), " @ ", rep(levels(subdat$Covarf), length(rownames(CMAT))), sep="")
		attr(SCIs2, which="caption") <-  paste("Simultaneous ", round(conf.level*100), "%-confidence intervals and adjusted p-values for differences for ",attr(CMAT, name="type") ,"-contrasts at each covariate level.", sep="")
	} else {
		fit2 <- NULL
		ANOVA2 <- NULL
		SCIs2 <- NULL
	}
	
	# ANOVA
	if (modelcomparison){
		anov <- as.data.frame(anova(fit1,fit2))
		rownames(anov) <- c("Model without interaction term", "Model with interaction term")
		attr(anov, which="caption") <- "ANOVA Table"
		
		aic <- AIC(fit1, fit2)
		rownames(aic) <- c("Model without interaction term", "Model with interaction term")
		attr(aic, which="caption") <- "AIC Table (smaller is better)"
	} else {
		anov <- NULL
		aic <- NULL
	}
	
	contr.text <- paste("Comparisons of multiple treatments by ", cmethod, "-type contrasts.", sep="")
	dist.text <- "Comparing estimates of a linear model under assumption of a normal distributed response, adjusting additionally for a time factor."
	proc.text <- paste("Procedure:","\n", contr.text, "\n", dist.text, "\n\n")
	
	Cova.text <- paste("Accounting for time covariate:\t", paste(CovarName, collapse=", "), "\n")
	resp.text <- paste("Response variable: \t\t", ResponseName, "\n")
	group.text <- paste("Group variable: \t\t", GroupName, "\n")
	confl.text <- paste("Confidence level: \t\t", conf.level, "\n")
	alt.text <- paste("Alternative: \t\t\t", alternative, "\n")
	tttext <- paste("Testing against the margin:\t", margin, "\n", sep="")
	
	TABLES <- list(ANOVA1, SCIs1, ANOVA2, SCIs2, anov, aic)
	TABLES <- TABLES[!unlist(lapply(TABLES,is.null))]
	
	
	if (nointeraction){
		nointt <- "Linear model without interaction term:\nANOVA:\n$XTABLE_1$\n\nMultiple comparisons for treatment levels:\n$XTABLE_2$ \n\n"
	} else {
		nointt <- ""
	}
	if (withinteraction){
		if (nointeraction){
			withintt <- "Linear model with interaction term:\nANOVA:\n$XTABLE_3$\n\nMultiple comparisons for treatment levels separately for each timepoint:\n$XTABLE_4$\n\n"
		} else {
			withintt <- "Linear model with interaction term:\nANOVA:\n$XTABLE_1$\n\nMultiple comparisons for treatment levels separately for each timepoint:\n$XTABLE_2$\n\n"
		}
	} else {
		withintt <- ""
	}
	if (modelcomparison){
		modelct <- "Model Comparison:\nANOVA:\n$XTABLE_5$\n\nAIC:\n$XTABLE_6$\n\n"
	} else {
		modelct <- ""
	}
	
	TEXT <- paste(
			"Comparisons: \n", resp.text, group.text, Cova.text, confl.text, alt.text, tttext, "\n\n",
			nointt,
			withintt,
			modelct,
			sep="")
	#cat(TEXT)
	
	out <- list()
	out$status <- 0
	out$error.msg <- error.msg
	out$headline <- "Multiple comparisons adjusting for time covariates."
	out$result <- TEXT
	out$xtables <- TABLES
	
	out$plotinfo$dat <- subdat
	out$plotinfo$ResponseName <- ResponseName
	out$plotinfo$CovarName <- CovarName
	out$plotinfo$GroupName <- GroupName
	out$plotinfo$alternative <- alternative
	out$plotinfo$margin <- margin
	out$plotinfo$nointeraction <- nointeraction
	out$plotinfo$withinteraction <- withinteraction
	out$plotinfo$modelcomparison <- modelcomparison
	out$plotinfo$fit1 <- fit1
	out$plotinfo$fit2 <- fit2
	out$plotinfo$SCI1 <- SCIs1
	out$plotinfo$SCI2 <- SCIs2
	out
}






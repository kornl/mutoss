\name{compareMutoss}
\title{Functions for comparing outputs of different procedures.}
\usage{compareMutoss(...)
mu.compare.adjusted(comparison.list= compareMutoss(mu.obj.1, mu.obj.2))
mu.compare.critical(comparison.list= compareMutoss(mu.obj.1, mu.obj.2))
mu.compare.summary(comparison.list= compareMutoss(mu.obj.1, mu.obj.2))}
\description{Functions for comparing outputs of different procedures.}
\details{These functions are used to compare the results of different multiple comparisons procedures stored as Mutoss class objects.
\code{compareMutoss} takes as input an arbitrary number of Mutoss objects and arranges them in a simple list objects (non S4).
\code{mu.compare.adjuted}, \code{mu.compare.critical} and \code{mu.compare.summary} take the output of the \code{compareMutoss} and plots or summerize the results textually or graphically.}
\value{\item{compareMutoss}{Returns a list with the following components:\cr
\bold{types}: Character vector of error types corrsponding to each procedure.\cr
\bold{rates}: Numeric vector of error rates used for each procedure.\cr
\bold{pi.nulls}: Numeric vector of estimates of the proportion of true null hypothesis if avilable.\cr
\bold{raw.pValues}: The raw p-values used for each procedure.\cr
\bold{adjusted.pvals}: Data frame with columns holding procedure specific adjusted p values.\cr
\bold{criticalValue}: Data frame with columns holding the critical values corresponding to each procedure and error rate.\cr
\bold{rejections}: Data frame with columns holding logical vectors of rejected hypotheses (TRUE for rejected).\cr}
\item{mu.compare.adjusted}{Creates a plot with the adjusted p-values for each procedure.}
\item{mu.compare.critical}{Creates a plot with the critical values for each procedure and error rate.}
\item{mu.compare.summary}{Creates a short textual summary for comparing results of different procedures.}}
\author{Jonathan Rosenblatt}
\alias{compareMutoss}
\alias{mu.compare.adjusted}
\alias{mu.compare.critical}
\alias{mu.compare.summary}
\arguments{\item{...}{An arbitrary number of Motoss class objects.}
\item{comparison.list}{The output of the \code{compareMutoss} function.}
\item{identify.check}{Logical parameter specifying if hypotheses should be identified on the output plots.}}
\examples{# TODO: EXAMPLE PROBLEMS
\dontrun{
\dontrun{Creating several Mutoss class objects}
mu.test.obj.1 <- mutoss.apply(new(Class="Mutoss", pValues=runif(10)), f=bonferroni, label="Bonferroni Correction", alpha=0.05, silent=T)#' 
mu.test.obj.2 <- mutoss.apply(new(Class="Mutoss", pValues=runif(10)), f=holm, label="Holm's step-down-procedure", alpha=0.05, silent=T)#' 
mu.test.obj.3 <- mutoss.apply(new(Class="Mutoss", pValues=runif(10)), f=aorc, label="Asymtotically optimal rejection curve", alpha=0.05, startIDX_SUD = 1, silent=T)
\dontrun{Trying to coercing a non-Mutoss object}
compareMutoss(1)
\dontrun{ Coercing several objects into a list}
compare.1<- compareMutoss(mu.test.obj.1, mu.test.obj.2)
compare.2<- compareMutoss(mu.test.obj.1, mu.test.obj.2, mu.test.obj.3)
\dontrun{Plotting the adjusted pvalues. Identification available.}
mu.compare.adjusted(compare.1, T)
mu.compare.adjusted(compare.2, T)
\dontrun{Plotting the critical values. Identification available.}
mu.compare.critical(compare.1, T)
mu.compare.critical(compare.2, T)
\dontrun{Showing a textual sumary}
mu.compare.summary(compare.1)
mu.compare.summary(compare.2)
}}

\name{SUD}
\alias{SUD}
\title{A general step-up-down procedure.}
\usage{SUD(pValues, criticalValues, startIDX_SUD)}
\description{A general step-up-down procedure.}
\details{Suppose we have n pValues and they are already sorted.
The procedure compares pValues[startIDX_SUD] with criticalValues[startIDX_SUD]
and then proceeds in step-up or step-down manner,   
depending on the result of this initial comparision.

If pValues[startIDX_SUD] <= criticalValues[startIDX_SUD], then the procedure
rejects the hypotheses associated with pValues[1], ..., pValues[startIDX_SUD]  
and carries on in a step-down manner from startIDX_SUD to n to reject additional hypotheses.

If pValues[startIDX_SUD] > criticalValues[startIDX_SUD], then the procedure
retains hypotheses associated with pValues[startIDX_SUD], ..., pValues[n] and 
carries on in a step-up manner with pValues[startIDX_SUD - 1], ..., pValues[1]. 

If startIDX_SUD equals n the algorithm behaves like a step-up procedure.

If startIDX_SUD equals 1 the algorithm behaves like a step-down procedure.}
\value{rejected logical vector indicating if hypotheses are rejected or retained.}
\author{MuToss-Coding Team}
\arguments{\item{pValues}{pValues to be used.}
\item{criticalValues}{criticalValues for the step-up-down procedure}
\item{startIDX_SUD}{the index (between 1 and length(pValues)) used for the first 
comparison of pValues[startIDX_SUD] and criticalValues[startIDX_SUD]. Depending
on the result of this comparison the algorithm decides to proceed in 
step-up or step-down manner.}}

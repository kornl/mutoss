\name{SD}
\alias{SD}
\title{A general step-down procedure.}
\usage{SD(pValues, criticalValues)}
\description{A general step-down procedure.}
\details{Suppose we have n pValues and they are already sorted.
The procedure starts with comparing pValues[1] with criticalValues[1]. If 
pValues[1] <= criticalValues[1], then the hypothsis associated with pValues[1] is rejected and the algorithm 
carries on with second smallest pValue and criticalValue in the same way. The algorithm stops
rejecting at the first index i for which pValues[i] > criticalValues[i]. 
Thus pValues[j] is rejected if and only if pValues[i] <= criticalValues[i] for all i <= j.}
\value{rejected logical vector indicating if hypotheses are rejected or retained.}
\author{MarselScheer}
\arguments{\item{pValues}{pValues to be used.}
\item{criticalValues}{criticalValues for the step-down procedure}}

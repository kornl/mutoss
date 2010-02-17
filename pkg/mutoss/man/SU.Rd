\name{SU}
\alias{SU}
\title{A general step-up procedure.}
\usage{SU(pValues, criticalValues)}
\description{A general step-up procedure.}
\details{Suppose we have n pValues and they are already sorted. 
The procedure starts with comparing pValues[n] with criticalValues[n]. If
pValues[n] > criticalValues[n], then the hypothesis associated with pValues[n] 
is retained and the algorithm carries on with the next pValue and criticalValue, here for example pValues[n-1]
and criticalValues[n-1]. The algorithm stops retaining at the first index i for
which pValues[i] <= criticalValues[i]. Thus pValues[j] is rejected if and only if
their exists an index i with j <= i and pValues[i] <= criticalValues[i].}
\value{rejected logical vector indicating if hypotheses are rejected or retained.}
\author{MarselScheer}
\arguments{\item{pValues}{pValues to be used.}
\item{criticalValues}{criticalValues for the step-up procedure}}

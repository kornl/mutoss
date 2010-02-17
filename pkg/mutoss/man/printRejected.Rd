\name{printRejected}
\alias{printRejected}
\title{Internal MuTossProjekt-Function}
\usage{printRejected(rejected, pValues=NULL, adjPValues=NULL)}
\description{Generates standard output for pValues, rejected and
adjustedPValues.}
\details{It generates an output on the console with the number of
hypotheses (number of pValues) and the number of rejected
hypotheses (number of rejected pValues). Further a data.frame
is constructed, one column containing the rejected pValues, 
one the index number of the rejected pValues and if given 
one column with the corresponding adjusted pValues.}
\author{MarselScheer}
\arguments{\item{rejected}{logical Vector indicating which pValue is rejected.}
\item{pValues}{the used pValues.}
\item{adjPValues}{the adjusted pValues.}}

\name{Mutoss-class}
\docType{class}
\alias{Mutoss-class}
\title{Class Mutoss}
\description{A Mutoss object can store the input for a multiple test procedure and also the output.}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Raw data used in model}
    \item{\code{model}:}{link function,error family and design}
    \item{\code{description}:}{a general description}
    \item{\code{statistic}:}{for Z, T or F statistics}
    \item{\code{hypotheses}:}{of class ANY}
    \item{\code{hypNames}:}{identifiers for the hypotheses tested}
    \item{\code{criticalValues}:}{procedure-specific critical values}
    \item{\code{pValues}:}{raw p-values}
    \item{\code{adjPValues}:}{procedure-specific adjusted p-values}
    \item{\code{errorControl}:}{A Mutoss S4 class of type \code{errorControl}}
    \item{\code{rejected}:}{Logical vector of the output of a procedure at a given error rate}
    \item{\code{qValues}:}{Storey's estimates of the supremum of the pFDR}
    \item{\code{locFDR}:}{Efron's local fdr estimates}
    \item{\code{pi0}:}{Estimate of the proportion of null hypotheses}
    \item{\code{confIntervals}:}{Confidence intervals for selected parameters}
    \item{\code{commandHistory}:}{commandHistory}
  }
}
\author{MuToss-Coding Team}
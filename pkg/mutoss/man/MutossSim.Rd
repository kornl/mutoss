\name{MutossSim-class}
\docType{class}
\alias{MutossSim-class}
\title{Class MutossSim}
\description{This class inherits the Mutoss-class. It is used in the simulation studies to keep
	track of the input and output of the procedures and the different parameters used for the
	data generating function and the parameters for the procedures used.}
\section{Slots}{
	\describe{
	    \item{\code{groundTruth}:}{a vector indicating which hypotheses are true resp. false}
	    \item{\code{parameters}:}{a list containing the parameters of the simulation}
	    \item{\code{idx}:}{if two MutossSim objects have the same idx, then they are based upon the same data}
	}
}
\author{MarselScheer}	


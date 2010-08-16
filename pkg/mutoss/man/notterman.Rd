\name{notterman}
\docType{data}
\alias{notterman}
\alias{notterman.grpLabel}
\alias{T.Test.tumor.vs.normal}
\title{Notterman data set}
\description{
The \code{notterman} data set is a data.frame containing 18 paired samples of 7457 gene expression values from Notterman et al. (2001).

The vector \code{notterman.grpLabel} contains 36 labels of type \code{Tumor} or \code{Normal} specifying the type of tissue for each column of the \code{notterman} data set.

The vector \code{T.Test.tumor.vs.normal} contains the resulting 7457 numeric p-values from the 7457 t-tests applied to each row of the data set.
}
\usage{notterman
notterman.grpLabel
T.Test.tumor.vs.normal
}
\format{
\itemize{
		\item{notterman - A data.frame containing 36 columns with 7457 observations}
		\item{notterman.grpLabel - A vector containing 36 labels of type \code{Tumor} or \code{Normal}}
		\item{T.Test.tumor.vs.normal - A vector containing 7457 numeric p-values}
	}
}
\source{D.A. Notterman, U. Alon, A.J. Sierk, and A.J. Levine:
\emph{Transcriptional Gene Expression Profiles of Colorectal
Adenoma, Adenocarcinoma, and Normal Tissue Examined by
Oligonucleotide Arrays}, Cancer Research, 2001, vol. 61, pp.
3124-3130.}
\keyword{datasets}

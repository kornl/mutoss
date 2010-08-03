\name{MutossMethod-class}
\docType{class}
\alias{MutossMethod-class}
\alias{MutossMethod}
\title{Class MutossMethod}
\description{A \code{MutossMethod} object describes a method that is applicable to \code{Mutoss} objects.}
\section{Slots}{  
  \describe{
    \item{\code{label}:}{A character string that contains the label that will be shown in menus.}
    \item{\code{errorControl}:}{One of the following character strings: FWER, FWER.weak, FDR, FDX, gFWER, perComparison.}
    \item{\code{callFunction}:}{A character string that contains the name of the Mutoss-compatible function.}        
    \item{\code{output}:}{A character vector of the \emph{possible} output of the function.}
    \item{\code{info}:}{A character string with info text. Should contain small description, author, reference etc..}
    \item{\code{assumptions}:}{A character vector of assumptions for this method.}
    \item{\code{parameters}:}{A list of optional description of parameters - see MuToss developer handbook.}
    \item{\code{list}:}{For extensions a list where you can put all your miscellaneous stuff.}
  }
}
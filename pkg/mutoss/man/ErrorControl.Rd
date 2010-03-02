\name{ErrorControl-class}
\docType{class}
\alias{ErrorControl-class}
\title{Class ErrorControl}
\description{This class holds the information related to the error rate}
\section{Slots}{
  \describe{
    \item{\code{type}:}{a character specifying the error rate. For example FWER, FDR, ... }
    \item{\code{alpha}:}{the level at which the error rate shall be controlled}
    \item{\code{k}:}{an additional parameter for generalised FWER}
    \item{\code{q}:}{an additional parameter for FDX (false discovery exceedance)}
  }
}
\author{MuToss-Coding Team}


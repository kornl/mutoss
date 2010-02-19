\name{oracleBH}
\alias{oracleBH}
\title{Bejamini-Hochberg (2000) oracle linear step-up Procedure...}
\usage{oracleBH(pValues, alpha, pi0, silent=FALSE)oracleBH(pValues, alpha, pi0, silent=FALSE)
oracleBH(pValues, alpha, pi0, silent=FALSE)
oracleBH(pValues, alpha, pi0, silent=FALSE)
oracleBH(pValues, alpha, pi0, silent=FALSE)
}
\description{Bejamini-Hochberg (2000) oracle linear step-up Procedure}
\details{Knowledge of the number of true null hypotheses (m0) can be very useful to improve upon the performance of the FDR controlling procedure. 
For the oracle linear step-up procedure we assume that m0 were given to us by an 'oracle', the linear step-up procedure with q0 = q*m/m0
would control the FDR at precisely the desired level q in the independent and continuous case, and
would then be more powerful in rejecting hypotheses for which the alternative holds.\code{oracleBH}: Knowledge of the number of true null hypotheses (m0) can be very useful to improve upon the performance of the FDR controlling procedure. 
For the oracle linear step-up procedure we assume that m0 were given to us by an 'oracle', the linear step-up procedure with q0 = q*m/m0
would control the FDR at precisely the desired level q in the independent and continuous case, and
would then be more powerful in rejecting hypotheses for which the alternative holds.


\code{oracleBH}: Knowledge of the number of true null hypotheses (m0) can be very useful to improve upon the performance of the FDR controlling procedure. 
For the oracle linear step-up procedure we assume that m0 were given to us by an 'oracle', the linear step-up procedure with q0 = q*m/m0
would control the FDR at precisely the desired level q in the independent and continuous case, and
would then be more powerful in rejecting hypotheses for which the alternative holds.


\code{oracleBH}: Knowledge of the number of true null hypotheses (m0) can be very useful to improve upon the performance of the FDR controlling procedure. 
For the oracle linear step-up procedure we assume that m0 were given to us by an 'oracle', the linear step-up procedure with q0 = q*m/m0
would control the FDR at precisely the desired level q in the independent and continuous case, and
would then be more powerful in rejecting hypotheses for which the alternative holds.


\code{oracleBH}: Knowledge of the number of true null hypotheses (m0) can be very useful to improve upon the performance of the FDR controlling procedure. 
For the oracle linear step-up procedure we assume that m0 were given to us by an 'oracle', the linear step-up procedure with q0 = q*m/m0
would control the FDR at precisely the desired level q in the independent and continuous case, and
would then be more powerful in rejecting hypotheses for which the alternative holds.


}
\value{A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test.}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}\code{oracleBH}: A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test.}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}


\code{oracleBH}: A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test.}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}


\code{oracleBH}: A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test.}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}


\code{oracleBH}: A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test.}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}


}
\author{HackNiklas}
\arguments{\item{pValues}{pValues to be used}
\item{alpha}{the level at which the FWER should be controlled}
\item{pi0}{miraculousy known number of true null hypotheses}
\item{silent}{Logical, if true any output on the console will be suppressed.}}
\examples{pval <- c(runif(50), runif(50, 0, 0.01))
result <- oracleBH(pValues=pval,alpha=0.05,pi0=0.85)pval <- c(runif(50), runif(50, 0, 0.01))
result <- oracleBH(pValues=pval,alpha=0.05,pi0=0.85)pval <- c(runif(50), runif(50, 0, 0.01))
result <- oracleBH(pValues=pval,alpha=0.05,pi0=0.85)pval <- c(runif(50), runif(50, 0, 0.01))
result <- oracleBH(pValues=pval,alpha=0.05,pi0=0.85)pval <- c(runif(50), runif(50, 0, 0.01))
result <- oracleBH(pValues=pval,alpha=0.05,pi0=0.85)}
\alias{oracleBH}

\alias{oracleBH}

\alias{oracleBH}

\alias{oracleBH}


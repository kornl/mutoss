\name{adaptiveSTS}
\alias{adaptiveSTS}
\title{Storey-Taylor-Siegmund (2004) adaptive step-up procedure}
\usage{adaptiveSTS(pValues, alpha, lambda=0.5, silent=FALSE)adaptiveSTS(pValues, alpha, lambda=0.5, silent=FALSE)
adaptiveSTS(pValues, alpha, lambda=0.5, silent=FALSE)
adaptiveSTS(pValues, alpha, lambda=0.5, silent=FALSE)
adaptiveSTS(pValues, alpha, lambda=0.5, silent=FALSE)
}
\description{Storey-Taylor-Siegmund's (2004) adaptive step-up procedure}
\details{The adaptive STS procedure uses a conservative estimate of pi0 which is 
plugged in a linear step-up procedure. The estimation of pi0 requires a 
parameter (lambda) which is set to 0.5 by default.
Note that the estimated pi0 is truncated at 1 as suggested by the author, 
so the implemetation of the procedure is not entirely supported by the proof in the reference.\code{adaptiveSTS}: The adaptive STS procedure uses a conservative estimate of pi0 which is 
plugged in a linear step-up procedure. The estimation of pi0 requires a 
parameter (lambda) which is set to 0.5 by default.
Note that the estimated pi0 is truncated at 1 as suggested by the author, 
so the implemetation of the procedure is not entirely supported by the proof in the reference.


\code{adaptiveSTS}: The adaptive STS procedure uses a conservative estimate of pi0 which is 
plugged in a linear step-up procedure. The estimation of pi0 requires a 
parameter (lambda) which is set to 0.5 by default.
Note that the estimated pi0 is truncated at 1 as suggested by the author, 
so the implemetation of the procedure is not entirely supported by the proof in the reference.


\code{adaptiveSTS}: The adaptive STS procedure uses a conservative estimate of pi0 which is 
plugged in a linear step-up procedure. The estimation of pi0 requires a 
parameter (lambda) which is set to 0.5 by default.
Note that the estimated pi0 is truncated at 1 as suggested by the author, 
so the implemetation of the procedure is not entirely supported by the proof in the reference.


\code{adaptiveSTS}: The adaptive STS procedure uses a conservative estimate of pi0 which is 
plugged in a linear step-up procedure. The estimation of pi0 requires a 
parameter (lambda) which is set to 0.5 by default.
Note that the estimated pi0 is truncated at 1 as suggested by the author, 
so the implemetation of the procedure is not entirely supported by the proof in the reference.


}
\value{A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}\code{adaptiveSTS}: A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}


\code{adaptiveSTS}: A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}


\code{adaptiveSTS}: A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}


\code{adaptiveSTS}: A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}


}
\author{Werft Wiebke}
\references{Storey, J.D., Taylor, J.E. and Siegmund, D. (2004). Strong control, conservative point estimation and
simultaneous conservative consistency of false discovery rates: a unified approach.
Journal of the Royal Statistical Society, B 66(1):187-205.Storey, J.D., Taylor, J.E. and Siegmund, D. (2004). Strong control, conservative point estimation and
simultaneous conservative consistency of false discovery rates: a unified approach.
Journal of the Royal Statistical Society, B 66(1):187-205.Storey, J.D., Taylor, J.E. and Siegmund, D. (2004). Strong control, conservative point estimation and
simultaneous conservative consistency of false discovery rates: a unified approach.
Journal of the Royal Statistical Society, B 66(1):187-205.Storey, J.D., Taylor, J.E. and Siegmund, D. (2004). Strong control, conservative point estimation and
simultaneous conservative consistency of false discovery rates: a unified approach.
Journal of the Royal Statistical Society, B 66(1):187-205.Storey, J.D., Taylor, J.E. and Siegmund, D. (2004). Strong control, conservative point estimation and
simultaneous conservative consistency of false discovery rates: a unified approach.
Journal of the Royal Statistical Society, B 66(1):187-205.}
\arguments{\item{pValues}{The used raw pValues.}
\item{alpha}{The level at which the FDR shall be controlled.}
\item{lambda}{The tuning parameter for the estimation procedure (defaults to 0.5)}
\item{silent}{If true any output on the console will be suppressed.}}
\examples{alpha <- 0.05
p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9,max=1))
result <- adaptiveSTS(p, alpha, lambda=0.5)
result <- adaptiveSTS(p, alpha, lambda=0.5, silent=TRUE)alpha <- 0.05
p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9,max=1))
result <- adaptiveSTS(p, alpha, lambda=0.5)
result <- adaptiveSTS(p, alpha, lambda=0.5, silent=TRUE)alpha <- 0.05
p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9,max=1))
result <- adaptiveSTS(p, alpha, lambda=0.5)
result <- adaptiveSTS(p, alpha, lambda=0.5, silent=TRUE)alpha <- 0.05
p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9,max=1))
result <- adaptiveSTS(p, alpha, lambda=0.5)
result <- adaptiveSTS(p, alpha, lambda=0.5, silent=TRUE)alpha <- 0.05
p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9,max=1))
result <- adaptiveSTS(p, alpha, lambda=0.5)
result <- adaptiveSTS(p, alpha, lambda=0.5, silent=TRUE)}
\alias{adaptiveSTS}

\alias{adaptiveSTS}

\alias{adaptiveSTS}

\alias{adaptiveSTS}


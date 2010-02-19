\name{BL}
\alias{BL}
\title{Benjamini-Liu (1999) step-down procedure}
\usage{BL(pValues, alpha, silent=FALSE)BL(pValues, alpha, silent=FALSE)
BL(pValues, alpha, silent=FALSE)
BL(pValues, alpha, silent=FALSE)
BL(pValues, alpha, silent=FALSE)
}
\description{Benjamini-Liu's step-down procedure is applied to pValues. 
The procedure controls the FDR if the corresponding test statistics are stochastically independent.}
\details{The Benjamini-Liu (BL) step-down procedure neither dominates nor is dominated by the Benjamini-Hochberg (BH) step-up procedure.
However, in Benjamini and Liu (1999) a large simulation study concerning the power of the two procedures reveals that the BL step-down procedure is more suitable when the number of hypotheses is small. 
Moreover, if most hypotheses are far from the null then the BL step-down procedure is more powerful than the BH step-up method.
The BL step-down method calculates critical values according to Benjamin and Liu (1999), 
i.e., c_i = 1 - (1 - min(1, (m*alpha)/(m-i+1)))^(1/(m-i+1)) for i = 1,...,m, where m is the number of hypotheses tested. 
Then, let k be the smallest i for which P_(i) > c_i and reject the associated hypotheses H_(1),...,H_(k-1).\code{BL}: The Benjamini-Liu (BL) step-down procedure neither dominates nor is dominated by the Benjamini-Hochberg (BH) step-up procedure.
However, in Benjamini and Liu (1999) a large simulation study concerning the power of the two procedures reveals that the BL step-down procedure is more suitable when the number of hypotheses is small. 
Moreover, if most hypotheses are far from the null then the BL step-down procedure is more powerful than the BH step-up method.
The BL step-down method calculates critical values according to Benjamin and Liu (1999), 
i.e., c_i = 1 - (1 - min(1, (m*alpha)/(m-i+1)))^(1/(m-i+1)) for i = 1,...,m, where m is the number of hypotheses tested. 
Then, let k be the smallest i for which P_(i) > c_i and reject the associated hypotheses H_(1),...,H_(k-1).


\code{BL}: The Benjamini-Liu (BL) step-down procedure neither dominates nor is dominated by the Benjamini-Hochberg (BH) step-up procedure.
However, in Benjamini and Liu (1999) a large simulation study concerning the power of the two procedures reveals that the BL step-down procedure is more suitable when the number of hypotheses is small. 
Moreover, if most hypotheses are far from the null then the BL step-down procedure is more powerful than the BH step-up method.
The BL step-down method calculates critical values according to Benjamin and Liu (1999), 
i.e., c_i = 1 - (1 - min(1, (m*alpha)/(m-i+1)))^(1/(m-i+1)) for i = 1,...,m, where m is the number of hypotheses tested. 
Then, let k be the smallest i for which P_(i) > c_i and reject the associated hypotheses H_(1),...,H_(k-1).


\code{BL}: The Benjamini-Liu (BL) step-down procedure neither dominates nor is dominated by the Benjamini-Hochberg (BH) step-up procedure.
However, in Benjamini and Liu (1999) a large simulation study concerning the power of the two procedures reveals that the BL step-down procedure is more suitable when the number of hypotheses is small. 
Moreover, if most hypotheses are far from the null then the BL step-down procedure is more powerful than the BH step-up method.
The BL step-down method calculates critical values according to Benjamin and Liu (1999), 
i.e., c_i = 1 - (1 - min(1, (m*alpha)/(m-i+1)))^(1/(m-i+1)) for i = 1,...,m, where m is the number of hypotheses tested. 
Then, let k be the smallest i for which P_(i) > c_i and reject the associated hypotheses H_(1),...,H_(k-1).


\code{BL}: The Benjamini-Liu (BL) step-down procedure neither dominates nor is dominated by the Benjamini-Hochberg (BH) step-up procedure.
However, in Benjamini and Liu (1999) a large simulation study concerning the power of the two procedures reveals that the BL step-down procedure is more suitable when the number of hypotheses is small. 
Moreover, if most hypotheses are far from the null then the BL step-down procedure is more powerful than the BH step-up method.
The BL step-down method calculates critical values according to Benjamin and Liu (1999), 
i.e., c_i = 1 - (1 - min(1, (m*alpha)/(m-i+1)))^(1/(m-i+1)) for i = 1,...,m, where m is the number of hypotheses tested. 
Then, let k be the smallest i for which P_(i) > c_i and reject the associated hypotheses H_(1),...,H_(k-1).


}
\value{A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues.}

\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test.} 

\item{rejected}{A logical vector indicating which hypotheses are rejected.}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}\code{BL}: A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues.}

\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test.} 

\item{rejected}{A logical vector indicating which hypotheses are rejected.}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}


\code{BL}: A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues.}

\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test.} 

\item{rejected}{A logical vector indicating which hypotheses are rejected.}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}


\code{BL}: A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues.}

\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test.} 

\item{rejected}{A logical vector indicating which hypotheses are rejected.}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}


\code{BL}: A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues.}

\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test.} 

\item{rejected}{A logical vector indicating which hypotheses are rejected.}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}


}
\author{Werft Wiebke}
\references{Bejamini, Y. and Liu, W. (1999). A step-down multiple hypotheses testing procedure that controls the false discovery rate under independence. 
Journal of Statistical Planning and Inference Vol. 82(1-2): 163-170.Bejamini, Y. and Liu, W. (1999). A step-down multiple hypotheses testing procedure that controls the false discovery rate under independence. 
Journal of Statistical Planning and Inference Vol. 82(1-2): 163-170.Bejamini, Y. and Liu, W. (1999). A step-down multiple hypotheses testing procedure that controls the false discovery rate under independence. 
Journal of Statistical Planning and Inference Vol. 82(1-2): 163-170.Bejamini, Y. and Liu, W. (1999). A step-down multiple hypotheses testing procedure that controls the false discovery rate under independence. 
Journal of Statistical Planning and Inference Vol. 82(1-2): 163-170.Bejamini, Y. and Liu, W. (1999). A step-down multiple hypotheses testing procedure that controls the false discovery rate under independence. 
Journal of Statistical Planning and Inference Vol. 82(1-2): 163-170.}
\arguments{\item{pValues}{Numeric vector of p-values}
\item{alpha}{The level at which the FDR is to be controlled.}
\item{silent}{If true any output on the console will be suppressed.}}
\examples{alpha <- 0.05
p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9, max=1))
result <- BL(p, alpha)
result <- BL(p, alpha, silent=TRUE)alpha <- 0.05
p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9, max=1))
result <- BL(p, alpha)
result <- BL(p, alpha, silent=TRUE)alpha <- 0.05
p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9, max=1))
result <- BL(p, alpha)
result <- BL(p, alpha, silent=TRUE)alpha <- 0.05
p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9, max=1))
result <- BL(p, alpha)
result <- BL(p, alpha, silent=TRUE)alpha <- 0.05
p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9, max=1))
result <- BL(p, alpha)
result <- BL(p, alpha, silent=TRUE)}
\alias{BL}

\alias{BL}

\alias{BL}

\alias{BL}


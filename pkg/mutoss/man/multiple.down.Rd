\name{multiple.down}
\alias{multiple.down}
\title{Benjamini-Krieger-Yekutieli (2006) Multi-Stage Step-Down}
\usage{multiple.down(pValues, alpha)multiple.down(pValues, alpha)
multiple.down(pValues, alpha)
multiple.down(pValues, alpha)
multiple.down(pValues, alpha)
}
\description{A p-value procedure which controls the FDR for independent test statistics.}
\details{A non-linear step-down p-value procedure which control the FDR for independent test 
statistics and enjoys more power then other non-adaptive procedure such as the linear step-up (BH).
For the case of non-independent test statistics, non-adaptive procedures such as the 
linear step-up (BH) or the all-purpose conservative Benjamini-Yekutieli (2001) are recommended.\code{multiple.down}: A non-linear step-down p-value procedure which control the FDR for independent test 
statistics and enjoys more power then other non-adaptive procedure such as the linear step-up (BH).
For the case of non-independent test statistics, non-adaptive procedures such as the 
linear step-up (BH) or the all-purpose conservative Benjamini-Yekutieli (2001) are recommended.


\code{multiple.down}: A non-linear step-down p-value procedure which control the FDR for independent test 
statistics and enjoys more power then other non-adaptive procedure such as the linear step-up (BH).
For the case of non-independent test statistics, non-adaptive procedures such as the 
linear step-up (BH) or the all-purpose conservative Benjamini-Yekutieli (2001) are recommended.


\code{multiple.down}: A non-linear step-down p-value procedure which control the FDR for independent test 
statistics and enjoys more power then other non-adaptive procedure such as the linear step-up (BH).
For the case of non-independent test statistics, non-adaptive procedures such as the 
linear step-up (BH) or the all-purpose conservative Benjamini-Yekutieli (2001) are recommended.


\code{multiple.down}: A non-linear step-down p-value procedure which control the FDR for independent test 
statistics and enjoys more power then other non-adaptive procedure such as the linear step-up (BH).
For the case of non-independent test statistics, non-adaptive procedures such as the 
linear step-up (BH) or the all-purpose conservative Benjamini-Yekutieli (2001) are recommended.


}
\value{A list containing:
\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test.} 

\item{adjPValues}{A numeric vector containing adjusted p-values.}

\item{pi0}{An estimate of the proportion of true null hypotheses among all hypotheses (pi0=m0/m). }

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}\code{multiple.down}: A list containing:
\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test.} 

\item{adjPValues}{A numeric vector containing adjusted p-values.}

\item{pi0}{An estimate of the proportion of true null hypotheses among all hypotheses (pi0=m0/m). }

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}


\code{multiple.down}: A list containing:
\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test.} 

\item{adjPValues}{A numeric vector containing adjusted p-values.}

\item{pi0}{An estimate of the proportion of true null hypotheses among all hypotheses (pi0=m0/m). }

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}


\code{multiple.down}: A list containing:
\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test.} 

\item{adjPValues}{A numeric vector containing adjusted p-values.}

\item{pi0}{An estimate of the proportion of true null hypotheses among all hypotheses (pi0=m0/m). }

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}


\code{multiple.down}: A list containing:
\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test.} 

\item{adjPValues}{A numeric vector containing adjusted p-values.}

\item{pi0}{An estimate of the proportion of true null hypotheses among all hypotheses (pi0=m0/m). }

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}


}
\author{Jonathan Rosenblatt}
\arguments{\item{pValues}{A numeric vector of p-values}
\item{alpha}{The FDR error rate to control}}
\examples{pvals<- runif(100)^2
alpha<- 0.2
result<- multiple.down(pvals, alpha)
result 
plot(result[['criticalValues']]~pvals)
plot(result[['adjPValues']]~pvals)
abline(v=alpha)pvals<- runif(100)^2
alpha<- 0.2
result<- multiple.down(pvals, alpha)
result 
plot(result[['criticalValues']]~pvals)
plot(result[['adjPValues']]~pvals)
abline(v=alpha)pvals<- runif(100)^2
alpha<- 0.2
result<- multiple.down(pvals, alpha)
result 
plot(result[['criticalValues']]~pvals)
plot(result[['adjPValues']]~pvals)
abline(v=alpha)pvals<- runif(100)^2
alpha<- 0.2
result<- multiple.down(pvals, alpha)
result 
plot(result[['criticalValues']]~pvals)
plot(result[['adjPValues']]~pvals)
abline(v=alpha)pvals<- runif(100)^2
alpha<- 0.2
result<- multiple.down(pvals, alpha)
result 
plot(result[['criticalValues']]~pvals)
plot(result[['adjPValues']]~pvals)
abline(v=alpha)}
\alias{multiple.down}

\alias{multiple.down}

\alias{multiple.down}

\alias{multiple.down}


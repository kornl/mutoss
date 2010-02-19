\name{pval2locfdr}
\alias{pval2locfdr}
\title{Strimmer et al.'s fdrtool-based local fdr}
\usage{pval2locfdr(pValues, cutoff)pval2locfdr(pValues, cutoff)
pval2locfdr(pValues, cutoff)
pval2locfdr(pValues, cutoff)
pval2locfdr(pValues, cutoff)
}
\description{The function \code{pval2locfdr} takes a vector of p-values and
estimates for each case the local fdr.}
\value{A list containing:

\item{locfdr}{Numeric vector with local FDR values for each case}
\item{rejected}{Logical vector indicating rejection/retention for each hypothesis when a \code{cutoff} is supplied.}\code{pval2locfdr}: A list containing:

\item{locfdr}{Numeric vector with local FDR values for each case}
\item{rejected}{Logical vector indicating rejection/retention for each hypothesis when a \code{cutoff} is supplied.}


\code{pval2locfdr}: A list containing:

\item{locfdr}{Numeric vector with local FDR values for each case}
\item{rejected}{Logical vector indicating rejection/retention for each hypothesis when a \code{cutoff} is supplied.}


\code{pval2locfdr}: A list containing:

\item{locfdr}{Numeric vector with local FDR values for each case}
\item{rejected}{Logical vector indicating rejection/retention for each hypothesis when a \code{cutoff} is supplied.}


\code{pval2locfdr}: A list containing:

\item{locfdr}{Numeric vector with local FDR values for each case}
\item{rejected}{Logical vector indicating rejection/retention for each hypothesis when a \code{cutoff} is supplied.}


}
\author{JonathanRosenblatt}
\references{Strimmer, K. (2008). fdrtool: a versatile R package for estimating local and tail
area-based false discovery rates. Bioinformatics 24: 1461-1462. \cr
Efron B., Tibshirani R., Storey J. D. and Tusher, V. (2001). Empirical Bayes Analysis of a Microarray Experiment.
Journal of the American Statistical Association 96(456):1151-1160.Strimmer, K. (2008). fdrtool: a versatile R package for estimating local and tail
area-based false discovery rates. Bioinformatics 24: 1461-1462. \cr
Efron B., Tibshirani R., Storey J. D. and Tusher, V. (2001). Empirical Bayes Analysis of a Microarray Experiment.
Journal of the American Statistical Association 96(456):1151-1160.Strimmer, K. (2008). fdrtool: a versatile R package for estimating local and tail
area-based false discovery rates. Bioinformatics 24: 1461-1462. \cr
Efron B., Tibshirani R., Storey J. D. and Tusher, V. (2001). Empirical Bayes Analysis of a Microarray Experiment.
Journal of the American Statistical Association 96(456):1151-1160.Strimmer, K. (2008). fdrtool: a versatile R package for estimating local and tail
area-based false discovery rates. Bioinformatics 24: 1461-1462. \cr
Efron B., Tibshirani R., Storey J. D. and Tusher, V. (2001). Empirical Bayes Analysis of a Microarray Experiment.
Journal of the American Statistical Association 96(456):1151-1160.Strimmer, K. (2008). fdrtool: a versatile R package for estimating local and tail
area-based false discovery rates. Bioinformatics 24: 1461-1462. \cr
Efron B., Tibshirani R., Storey J. D. and Tusher, V. (2001). Empirical Bayes Analysis of a Microarray Experiment.
Journal of the American Statistical Association 96(456):1151-1160.}
\arguments{\item{pValues}{pValues to be used.}
\item{cutoff}{The local fdr cutoff for rejection. Hypotheses with \code{local fdr} smaller then \code{cutoff} will be rejected.}}
\examples{pvals<- runif(1000)^2
pval2locfdr(pvals)
pval2locfdr(pValues=pvals, cutoff=0.4)pvals<- runif(1000)^2
pval2locfdr(pvals)
pval2locfdr(pValues=pvals, cutoff=0.4)pvals<- runif(1000)^2
pval2locfdr(pvals)
pval2locfdr(pValues=pvals, cutoff=0.4)pvals<- runif(1000)^2
pval2locfdr(pvals)
pval2locfdr(pValues=pvals, cutoff=0.4)pvals<- runif(1000)^2
pval2locfdr(pvals)
pval2locfdr(pValues=pvals, cutoff=0.4)}
\alias{pval2locfdr}

\alias{pval2locfdr}

\alias{pval2locfdr}

\alias{pval2locfdr}


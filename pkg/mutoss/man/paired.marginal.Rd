\name{paired.marginal}
\alias{paired.marginal}
\title{Marginal paired two sample test}
\usage{paired.marginal(data, model, robust, alternative, psi0, equalvar)}
\description{The robust version uses the Wilcoxon signed rank test, otherwise a paired t-test will be performed.}
\details{A vector of classlabels needs to be provided to distinguish the two paired groups. The arrangement of group indices does not matter, as long
		as the columns are arranged in the same corresponding order between groups. For example, if group 1 is code as 0 and group 2 is 
		coded as 1, for 3 pairs of data, it does not matter if the classlabel is coded as (0,0,0,1,1,1) or (1,1,1,0,0,0) or (0,1,0,1,0,1)
		or (1,0,1,0,1,0), the paired differences between groups will be calculated as group2 - group1.}
\value{
\item{pValues}{A numeric vector containing the unadjusted pValues}
}
\author{MuToss-Coding Team}
\references{
  Wilcoxon, F. (1945). Individual Comparisons by Ranking Methods. Biometrics Bulletin 1:80-83.
} 
 \arguments{
  \item{data}{the data set}
  \item{model}{the result of a call to \code{paired.model(classlabel)}}
  \item{robust}{a logical variable indicating whether a paired t-test or a Wilcoxon signed rank test should be used.}
  \item{alternative}{a character string specifying the alternative hypothesis,
          must be one of \code{two.sided}, \code{greater} or \code{less}}
  \item{psi0}{a numeric that defines the hypothesized null value}
  \item{equalvar}{a logical variable indicating whether to treat the two variances as being equal}
}

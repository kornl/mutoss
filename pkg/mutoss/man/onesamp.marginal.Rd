\name{onesamp.marginal}
\alias{onesamp.marginal}
\title{Marginal one sample test}
\usage{onesamp.marginal(data, robust, alternative, psi0)}
\description{The robust version uses the Wilcoxon-Mann-Whitney test, otherwise a t-test will be performed.}
\references{
  Wilcoxon, F. (1945). Individual Comparisons by Ranking Methods. Biometrics Bulletin 1:80-83.
  Mann, H. and Whitney, D. (1947). On a test of whether one of two random variables is stochastically larger than the other. Annals of Mathematical Statistics 18:50-60
  Student (1908). The probable error of a mean. Biometrika, 6(1):1-25.
}
 \arguments{
  \item{data}{the data set}
  \item{robust}{a logical variable indicating whether a t-test or a Wilcoxon-Mann-Whitney test should be used.}
  \item{alternative}{a character string specifying the alternative hypothesis,
          must be one of \code{two.sided}, \code{greater} or
          \code{less}}
  \item{psi0}{a numeric that defines the hypothesized null value}
}
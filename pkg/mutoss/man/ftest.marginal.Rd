\name{ftest.marginal}
\alias{ftest.marginal}
\title{Marginal F test}
\usage{ftest.marginal(data, model, robust)}
\description{The robust version uses the Kruskal-Wallis test, otherwise a F-test will be performed.}
\details{A classlabel needs to be provided to distinguish k sample groups.}
\value{\item{pValues}{A numeric vector containing the unadjusted pValues}}
\author{MuToss-Coding Team}
\references{Kruskal, W.H. und Wallis, W.A. (1952). Use of ranks in one-criterion variance analysis. JASA, 47:583-621}
\arguments{
  \item{data}{the data set}
  \item{model}{the result of a call to \code{ftest.model(classlabel)}}
  \item{robust}{a logical variable indicating whether a F-test or a Kruskal-Wallis test should be used.}
}

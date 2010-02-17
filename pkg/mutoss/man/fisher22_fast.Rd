\name{fisher22_fast}
\alias{fisher22_fast}
\title{Fisher (2x2) table association analysis for calculating one (marginal) p-value...}
\usage{fisher22_fast(obs, epsilon)}
\description{Fisher (2x2) table association analysis for calculating one (marginal) p-value}
\value{A list containing:

\item{nonrand_p}{The non-randomized (conservative) p-value}

\item{rand_p}{The randomized (non-conservative) p-value}

\item{prob_table}{The conditional probability of the observed table (given the merginals)}}
\author{ThorstenDickhaus}
\arguments{\item{obs}{The observed (2x2) table}
\item{epsilon}{A threshold for comparing real numbers to zero}}

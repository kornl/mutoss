\name{jointCDF.orderedUnif}
\alias{jointCDF.orderedUnif}
\title{Joint cumulative distribution function of order statistics of n iid. U(0,1)-distributed random variables}
\usage{jointCDF.orderedUnif(vec)}
\description{Calculates the joint cumulative distribution function of order statistics 
of n iid. U(0,1)-distributed random variables at argument vec. 
Because of numerical issues n should not be greater than 100.}
\details{Following Shorack, Wellner (1986) or Finner, Roters (2002) by applying
Bolshev's recursion the joint distribution is calculated.}
\value{The return value is the following probability 
P(U_(1:n) <= vec[1], ..., U_(n:n) <= vec[n]), where
U_1, ..., U_n are assumed to be iid. uniformly 
distributed on [0,1]. The i-th ordered value is denoted by
U_(i:n) and n equals length(vec)}
\references{Shorack, G. R. and Wellner, J. A. (1986). 
Empirical Processes with Applications to Statistics.
Wiley, New York.

Finner, H. and Roters, M. (2002). Multiple hypotheses testing and
expected type I errors. Ann. Statist. 30, 220-238.}
\author{MarselScheer}
\arguments{\item{vec}{a numeric vector. The length of the vector also
determines the number of random variables considered.}}

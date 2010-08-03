\name{reject}
\alias{reject}
\title{reject}
\usage{reject(sorted, criticals)}
\description{
	Returns the highest rejected p-value and its index given some critical values.
}
\arguments{
  \item{sorted}{
	Sorted p-values
  }
  \item{criticals}{
  	Critical values
  }
}
\value{
	A list with elements
\item{cutoff}{highest rejected p-value}
\item{cut.index }{index of highest rejected p-value}
}
\name{requireLibrary}
\alias{requireLibrary}
\title{Tries to load a package.}
\usage{requireLibrary(package)}
\description{Tries to load a package.
If this package does not exist, it will ask the user whether the package should be installed and loaded.
If the user negates, we will raise an error via stop.}
\value{NULL}
\author{MuToss-Coding Team}
\arguments{\item{package}{Package to load}}

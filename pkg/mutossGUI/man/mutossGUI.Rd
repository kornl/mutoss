\name{mutossGUI}
\alias{mutossGUI}
\title{Function to start the MuTossGUI Java class}
\description{
  This function executes a method of a Java class
  defined in a JAR inside the R package.
}
\usage{
  mutossGUI(debugOutput=FALSE)
}

\arguments{
  \item{debugOutput}{
	If false (the default) the verbose output for debugging will only be written to the file mutoss.log in the java temporary directory (\code{System.getProperty("java.io.tmpdir")}).
	If set to true the output is also printed to the R Console.
	Note that the behavior can only be influenced the first time the GUI is called in an R session. After that this parameter is ignored.
  }
}
\details{
  Details are given in the package vignette.
}
\value{
  None (invisible \code{NULL}).
}
\author{MuToss Coding Team}
\examples{
  \dontrun{mutossGUI()}
}
\keyword{manip}
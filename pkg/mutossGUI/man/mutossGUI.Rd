\name{mutossGUI}
\alias{mutossGUI}
\title{Function to start the MuTossGUI Java class}
\description{
  Starts the graphical user interface for the MuToss system.
}
\details{
  This function executes the method \code{startGUI()} of the Java class \code{MuTossGUI}
  defined in the JAR file \file{inst/java/mutossgui.jar} inside the R package. 
  Further details are given in the package vignette.
}
\usage{
  mutossGUI(debugOutput=FALSE)
}
\arguments{
  \item{debugOutput}{
	If \code{FALSE} (the default) the verbose output for debugging will only be written to the file mutoss.log in the java temporary directory (\code{System.getProperty("java.io.tmpdir")}).
	If set to \code{TRUE} the output is also printed to the R Console.
	Note that the behavior can only be influenced the first time the GUI is called in an R session. After that this parameter is ignored.
  }
}
\value{
  None (invisible \code{NULL}).
}
\author{
Kornelius Rohmeyer \email{rohmeyer@small-projects.de}
}
\examples{
  \dontrun{
  mutossGUI()
  }
}
\keyword{manip}
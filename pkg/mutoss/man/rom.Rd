\name{rom}
\alias{rom}
\title{Rom's (1990) step-up-procedure.}
\usage{rom(pValues, alpha, silent=FALSE)rom(pValues, alpha, silent=FALSE)
rom(pValues, alpha, silent=FALSE)
rom(pValues, alpha, silent=FALSE)
rom(pValues, alpha, silent=FALSE)
}
\description{Rom's step-up-procedure is applied to pValues. The procedure 
controls the FWER in the strong sense if the pValues are
stochastically independent.}
\details{This function calculates the critical values by the formula given
in Finner, H. and Roters, M. (2002) based on the joint distribution
of order statistics. After that a step-up test
is performed to reject hypotheses associated with pValues.

Since the formula for the critical values is recursive,
the calculation of adjusted pValues is far from obvious and is
not implemented here.\code{rom}: This function calculates the critical values by the formula given
in Finner, H. and Roters, M. (2002) based on the joint distribution
of order statistics. After that a step-up test
is performed to reject hypotheses associated with pValues.

Since the formula for the critical values is recursive,
the calculation of adjusted pValues is far from obvious and is
not implemented here.


\code{rom}: This function calculates the critical values by the formula given
in Finner, H. and Roters, M. (2002) based on the joint distribution
of order statistics. After that a step-up test
is performed to reject hypotheses associated with pValues.

Since the formula for the critical values is recursive,
the calculation of adjusted pValues is far from obvious and is
not implemented here.


\code{rom}: This function calculates the critical values by the formula given
in Finner, H. and Roters, M. (2002) based on the joint distribution
of order statistics. After that a step-up test
is performed to reject hypotheses associated with pValues.

Since the formula for the critical values is recursive,
the calculation of adjusted pValues is far from obvious and is
not implemented here.


\code{rom}: This function calculates the critical values by the formula given
in Finner, H. and Roters, M. (2002) based on the joint distribution
of order statistics. After that a step-up test
is performed to reject hypotheses associated with pValues.

Since the formula for the critical values is recursive,
the calculation of adjusted pValues is far from obvious and is
not implemented here.


}
\value{A list containing:
\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected.}

\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test.}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}\code{rom}: A list containing:
\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected.}

\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test.}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}


\code{rom}: A list containing:
\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected.}

\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test.}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}


\code{rom}: A list containing:
\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected.}

\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test.}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}


\code{rom}: A list containing:
\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected.}

\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test.}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}


}
\author{Marsel Scheer}
\references{Rom, D. M. (1990). A sequentially rejective test 
procedure based on a modified Bonferroni inequality. 
Biometrika 77, 663-665.

Finner, H. and Roters, M. (2002). Multiple hypotheses testing and
expected type I errors. Ann. Statist. 30, 220-238.Rom, D. M. (1990). A sequentially rejective test 
procedure based on a modified Bonferroni inequality. 
Biometrika 77, 663-665.

Finner, H. and Roters, M. (2002). Multiple hypotheses testing and
expected type I errors. Ann. Statist. 30, 220-238.Rom, D. M. (1990). A sequentially rejective test 
procedure based on a modified Bonferroni inequality. 
Biometrika 77, 663-665.

Finner, H. and Roters, M. (2002). Multiple hypotheses testing and
expected type I errors. Ann. Statist. 30, 220-238.Rom, D. M. (1990). A sequentially rejective test 
procedure based on a modified Bonferroni inequality. 
Biometrika 77, 663-665.

Finner, H. and Roters, M. (2002). Multiple hypotheses testing and
expected type I errors. Ann. Statist. 30, 220-238.Rom, D. M. (1990). A sequentially rejective test 
procedure based on a modified Bonferroni inequality. 
Biometrika 77, 663-665.

Finner, H. and Roters, M. (2002). Multiple hypotheses testing and
expected type I errors. Ann. Statist. 30, 220-238.}
\arguments{\item{pValues}{pValues to be used. They are assumed to be stochastically independent.}
\item{alpha}{the level at which the FWER shall be controlled.}
\item{silent}{if true any output on the console will be suppressed.}}
\examples{r <- c(runif(50), runif(50, 0, 0.01))
result <- rom(r, 0.05)
result <- rom(r, 0.05, silent = TRUE)r <- c(runif(50), runif(50, 0, 0.01))
result <- rom(r, 0.05)
result <- rom(r, 0.05, silent = TRUE)r <- c(runif(50), runif(50, 0, 0.01))
result <- rom(r, 0.05)
result <- rom(r, 0.05, silent = TRUE)r <- c(runif(50), runif(50, 0, 0.01))
result <- rom(r, 0.05)
result <- rom(r, 0.05, silent = TRUE)r <- c(runif(50), runif(50, 0, 0.01))
result <- rom(r, 0.05)
result <- rom(r, 0.05, silent = TRUE)}
\alias{rom}

\alias{rom}

\alias{rom}

\alias{rom}


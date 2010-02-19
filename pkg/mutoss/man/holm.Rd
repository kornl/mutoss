\name{holm}
\alias{holm}
\title{Holm's (1979) step-down-procedure}
\usage{holm(pValues, alpha, silent=FALSE)holm(pValues, alpha, silent=FALSE)
holm(pValues, alpha, silent=FALSE)
holm(pValues, alpha, silent=FALSE)
holm(pValues, alpha, silent=FALSE)
}
\description{Holm's step-down-procedure is applied to pValues. It controls
the FWER in the strong sense under arbitrary dependency.}
\details{Holm's procedure uses the same critical values as Hochberg's procedure, namely  c(i)=alpha/(m-i+1), 
but is a step-down version while Hochberg's method is a step-up version of the Bonferroni test.
Holm's method is based on the Bonferroni inequality and is valid regardless of the joint
distribution of the test statistics, whereas Hochberg's method relies on the assumption that 
Simes' inequality holds for the joint null distribution of the test statistics. If this assumption is met, Hochberg's
step-up procedure is more powerful than Holm's step-down procedure.\code{holm}: Holm's procedure uses the same critical values as Hochberg's procedure, namely  c(i)=alpha/(m-i+1), 
but is a step-down version while Hochberg's method is a step-up version of the Bonferroni test.
Holm's method is based on the Bonferroni inequality and is valid regardless of the joint
distribution of the test statistics, whereas Hochberg's method relies on the assumption that 
Simes' inequality holds for the joint null distribution of the test statistics. If this assumption is met, Hochberg's
step-up procedure is more powerful than Holm's step-down procedure.


\code{holm}: Holm's procedure uses the same critical values as Hochberg's procedure, namely  c(i)=alpha/(m-i+1), 
but is a step-down version while Hochberg's method is a step-up version of the Bonferroni test.
Holm's method is based on the Bonferroni inequality and is valid regardless of the joint
distribution of the test statistics, whereas Hochberg's method relies on the assumption that 
Simes' inequality holds for the joint null distribution of the test statistics. If this assumption is met, Hochberg's
step-up procedure is more powerful than Holm's step-down procedure.


\code{holm}: Holm's procedure uses the same critical values as Hochberg's procedure, namely  c(i)=alpha/(m-i+1), 
but is a step-down version while Hochberg's method is a step-up version of the Bonferroni test.
Holm's method is based on the Bonferroni inequality and is valid regardless of the joint
distribution of the test statistics, whereas Hochberg's method relies on the assumption that 
Simes' inequality holds for the joint null distribution of the test statistics. If this assumption is met, Hochberg's
step-up procedure is more powerful than Holm's step-down procedure.


\code{holm}: Holm's procedure uses the same critical values as Hochberg's procedure, namely  c(i)=alpha/(m-i+1), 
but is a step-down version while Hochberg's method is a step-up version of the Bonferroni test.
Holm's method is based on the Bonferroni inequality and is valid regardless of the joint
distribution of the test statistics, whereas Hochberg's method relies on the assumption that 
Simes' inequality holds for the joint null distribution of the test statistics. If this assumption is met, Hochberg's
step-up procedure is more powerful than Holm's step-down procedure.


}
\value{A list containing: 

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{criticalValues}{A numeric vector containing critical values used in the step-down test}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}\code{holm}: A list containing: 

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{criticalValues}{A numeric vector containing critical values used in the step-down test}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}


\code{holm}: A list containing: 

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{criticalValues}{A numeric vector containing critical values used in the step-down test}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}


\code{holm}: A list containing: 

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{criticalValues}{A numeric vector containing critical values used in the step-down test}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}


\code{holm}: A list containing: 

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{criticalValues}{A numeric vector containing critical values used in the step-down test}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}


}
\author{MarselScheer}
\references{S. Holm (1979). A simple sequentially rejective multiple 
test procedure. Scand. J. Statist. Vol. 6, 65-70. \eqn{n}

Huang, Y. and Hsu, J. (2007). Hochberg's step-up method: cutting corners off Holm's step-down method. Biometrika, 94(4):965-975.S. Holm (1979). A simple sequentially rejective multiple 
test procedure. Scand. J. Statist. Vol. 6, 65-70. \eqn{n}

Huang, Y. and Hsu, J. (2007). Hochberg's step-up method: cutting corners off Holm's step-down method. Biometrika, 94(4):965-975.S. Holm (1979). A simple sequentially rejective multiple 
test procedure. Scand. J. Statist. Vol. 6, 65-70. \eqn{n}

Huang, Y. and Hsu, J. (2007). Hochberg's step-up method: cutting corners off Holm's step-down method. Biometrika, 94(4):965-975.S. Holm (1979). A simple sequentially rejective multiple 
test procedure. Scand. J. Statist. Vol. 6, 65-70. \eqn{n}

Huang, Y. and Hsu, J. (2007). Hochberg's step-up method: cutting corners off Holm's step-down method. Biometrika, 94(4):965-975.S. Holm (1979). A simple sequentially rejective multiple 
test procedure. Scand. J. Statist. Vol. 6, 65-70. \eqn{n}

Huang, Y. and Hsu, J. (2007). Hochberg's step-up method: cutting corners off Holm's step-down method. Biometrika, 94(4):965-975.}
\arguments{\item{pValues}{pValues to be used. They can have arbitrary dependency structure.}
\item{alpha}{The level at which the FWER shall be controlled}
\item{silent}{If true any output on the console will be suppressed.}}
\examples{r <- c(runif(50), runif(50, 0, 0.01))
result 	<- holm(r, 0.05)
result 	<- holm(r, 0.05, silent = TRUE)r <- c(runif(50), runif(50, 0, 0.01))
result 	<- holm(r, 0.05)
result 	<- holm(r, 0.05, silent = TRUE)r <- c(runif(50), runif(50, 0, 0.01))
result 	<- holm(r, 0.05)
result 	<- holm(r, 0.05, silent = TRUE)r <- c(runif(50), runif(50, 0, 0.01))
result 	<- holm(r, 0.05)
result 	<- holm(r, 0.05, silent = TRUE)r <- c(runif(50), runif(50, 0, 0.01))
result 	<- holm(r, 0.05)
result 	<- holm(r, 0.05, silent = TRUE)}
\alias{holm}

\alias{holm}

\alias{holm}

\alias{holm}


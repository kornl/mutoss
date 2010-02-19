\name{hochberg}
\alias{hochberg}
\title{Hochberg (1988) step-up procedure}
\usage{hochberg(pValues, alpha, silent=FALSE)hochberg(pValues, alpha, silent=FALSE)
hochberg(pValues, alpha, silent=FALSE)
hochberg(pValues, alpha, silent=FALSE)
hochberg(pValues, alpha, silent=FALSE)
}
\description{The Hochberg step-up procedure is based on marginal p-values. It controls the FWER in the strong 
sense under joint null distributions of the test statistics that satisfy Simes' inequality.}
\details{The Hochberg procedure is more powerful than Holm's (1979) procedure, but the test statistics need to be
independent or have a distribution with multivariate total positivity of order two or a scale mixture
thereof for its validity (Sarkar, 1998).
Both procedures use the same set of critical values c(i)=alpha/(m-i+1). Whereas Holm's procedure is a step-down 
version of the Bonferroni test, and Hochberg's is a step-up version of the Bonferroni test.
Note that Holm's method is based on the Bonferroni inequality and is valid regardless of the joint
distribution of the test statistics.\code{hochberg}: The Hochberg procedure is more powerful than Holm's (1979) procedure, but the test statistics need to be
independent or have a distribution with multivariate total positivity of order two or a scale mixture
thereof for its validity (Sarkar, 1998).
Both procedures use the same set of critical values c(i)=alpha/(m-i+1). Whereas Holm's procedure is a step-down 
version of the Bonferroni test, and Hochberg's is a step-up version of the Bonferroni test.
Note that Holm's method is based on the Bonferroni inequality and is valid regardless of the joint
distribution of the test statistics.


\code{hochberg}: The Hochberg procedure is more powerful than Holm's (1979) procedure, but the test statistics need to be
independent or have a distribution with multivariate total positivity of order two or a scale mixture
thereof for its validity (Sarkar, 1998).
Both procedures use the same set of critical values c(i)=alpha/(m-i+1). Whereas Holm's procedure is a step-down 
version of the Bonferroni test, and Hochberg's is a step-up version of the Bonferroni test.
Note that Holm's method is based on the Bonferroni inequality and is valid regardless of the joint
distribution of the test statistics.


\code{hochberg}: The Hochberg procedure is more powerful than Holm's (1979) procedure, but the test statistics need to be
independent or have a distribution with multivariate total positivity of order two or a scale mixture
thereof for its validity (Sarkar, 1998).
Both procedures use the same set of critical values c(i)=alpha/(m-i+1). Whereas Holm's procedure is a step-down 
version of the Bonferroni test, and Hochberg's is a step-up version of the Bonferroni test.
Note that Holm's method is based on the Bonferroni inequality and is valid regardless of the joint
distribution of the test statistics.


\code{hochberg}: The Hochberg procedure is more powerful than Holm's (1979) procedure, but the test statistics need to be
independent or have a distribution with multivariate total positivity of order two or a scale mixture
thereof for its validity (Sarkar, 1998).
Both procedures use the same set of critical values c(i)=alpha/(m-i+1). Whereas Holm's procedure is a step-down 
version of the Bonferroni test, and Hochberg's is a step-up version of the Bonferroni test.
Note that Holm's method is based on the Bonferroni inequality and is valid regardless of the joint
distribution of the test statistics.


}
\value{A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}\code{hochberg}: A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}


\code{hochberg}: A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}


\code{hochberg}: A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}


\code{hochberg}: A list containing:

\item{adjPValues}{A numeric vector containing the adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected}

\item{criticalValues}{A numeric vector containing critical values used in the step-up-down test}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function and the level \code{alpha}.}


}
\author{WerftWiebke}
\references{Hochberg, Y. (1988). A sharper Bonferroni procedure for multiple tests of significance. 
Biometrika, 75:800-802.\eqn{n}

Huang, Y. and Hsu, J. (2007). Hochberg's step-up method: cutting corners off Holm's step-down method. Biometrika, 94(4):965-975.Hochberg, Y. (1988). A sharper Bonferroni procedure for multiple tests of significance. 
Biometrika, 75:800-802.\eqn{n}

Huang, Y. and Hsu, J. (2007). Hochberg's step-up method: cutting corners off Holm's step-down method. Biometrika, 94(4):965-975.Hochberg, Y. (1988). A sharper Bonferroni procedure for multiple tests of significance. 
Biometrika, 75:800-802.\eqn{n}

Huang, Y. and Hsu, J. (2007). Hochberg's step-up method: cutting corners off Holm's step-down method. Biometrika, 94(4):965-975.Hochberg, Y. (1988). A sharper Bonferroni procedure for multiple tests of significance. 
Biometrika, 75:800-802.\eqn{n}

Huang, Y. and Hsu, J. (2007). Hochberg's step-up method: cutting corners off Holm's step-down method. Biometrika, 94(4):965-975.Hochberg, Y. (1988). A sharper Bonferroni procedure for multiple tests of significance. 
Biometrika, 75:800-802.\eqn{n}

Huang, Y. and Hsu, J. (2007). Hochberg's step-up method: cutting corners off Holm's step-down method. Biometrika, 94(4):965-975.}
\arguments{\item{pValues}{The used raw pValues.}
\item{alpha}{The level at which the FDR shall be controlled.}
\item{silent}{If true any output on the console will be suppressed.}}
\examples{alpha <- 0.05
p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9,max=1))
result <- hochberg(p, alpha)
result <- hochberg(p, alpha, silent=TRUE)alpha <- 0.05
p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9,max=1))
result <- hochberg(p, alpha)
result <- hochberg(p, alpha, silent=TRUE)alpha <- 0.05
p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9,max=1))
result <- hochberg(p, alpha)
result <- hochberg(p, alpha, silent=TRUE)alpha <- 0.05
p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9,max=1))
result <- hochberg(p, alpha)
result <- hochberg(p, alpha, silent=TRUE)alpha <- 0.05
p <-c(runif(10, min=0, max=0.01), runif(10, min=0.9,max=1))
result <- hochberg(p, alpha)
result <- hochberg(p, alpha, silent=TRUE)}
\alias{hochberg}

\alias{hochberg}

\alias{hochberg}

\alias{hochberg}


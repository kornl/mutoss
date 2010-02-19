\name{calculateBetaAdjustment}
\alias{calculateBetaAdjustment}
\title{Calculating the beta adjustment factor for the asymptotically optimal rejection curve.}
\usage{calculateBetaAdjustment(n, startIDX_SUD, alpha, silent=FALSE,
    initialBeta=1, maxBinarySteps=50, tolerance=1e-04)calculateBetaAdjustment(n, startIDX_SUD, alpha, silent=FALSE, initialBeta=1, maxBinarySteps=50,
    tolerance=1e-04)
calculateBetaAdjustment(n, startIDX_SUD, alpha, silent=FALSE, initialBeta=1, maxBinarySteps=50,
    tolerance=1e-04)
calculateBetaAdjustment(n, startIDX_SUD, alpha, silent=FALSE, initialBeta=1, maxBinarySteps=50,
    tolerance=1e-04)
calculateBetaAdjustment(n, startIDX_SUD, alpha, silent=FALSE, initialBeta=1, maxBinarySteps=50,
    tolerance=1e-04)
}
\description{Calculates the beta to adjust the asymptotically optimal rejection curve
used by the function aorc() for a finite sample size. Then  
aorc(..., betaAdjustment = beta) controls the FDR also in the
finite sample situation.}
\details{The asymptotically optimal rejection curve, denoted by f(t), does not 
provide finite control of the FDR. calculateBetaAdjustment() calculates
a factor, denoted by beta, such that (1 + beta/n) * f(t) provides
finite control of the FDR.

The beta is calculated with the bisection approach. Assume there are beta1
and beta2 such that the choosing beta1 controls the FDR and beta2 not, then the 
optimal beta lies in [beta2, beta1]. If the choice (beta2 + beta1)/2 controls
the FDR, the optimal FDR lies in [(beta2 + beta1)/2, beta1]
and so on.\code{calculateBetaAdjustment}: The asymptotically optimal rejection curve, denoted by f(t), does not 
provide finite control of the FDR. calculateBetaAdjustment() calculates
a factor, denoted by beta, such that (1 + beta/n) * f(t) provides
finite control of the FDR.

The beta is calculated with the bisection approach. Assume there are beta1
and beta2 such that the choosing beta1 controls the FDR and beta2 not, then the 
optimal beta lies in [beta2, beta1]. If the choice (beta2 + beta1)/2 controls
the FDR, the optimal FDR lies in [(beta2 + beta1)/2, beta1]
and so on.


\code{calculateBetaAdjustment}: The asymptotically optimal rejection curve, denoted by f(t), does not 
provide finite control of the FDR. calculateBetaAdjustment() calculates
a factor, denoted by beta, such that (1 + beta/n) * f(t) provides
finite control of the FDR.

The beta is calculated with the bisection approach. Assume there are beta1
and beta2 such that the choosing beta1 controls the FDR and beta2 not, then the 
optimal beta lies in [beta2, beta1]. If the choice (beta2 + beta1)/2 controls
the FDR, the optimal FDR lies in [(beta2 + beta1)/2, beta1]
and so on.


\code{calculateBetaAdjustment}: The asymptotically optimal rejection curve, denoted by f(t), does not 
provide finite control of the FDR. calculateBetaAdjustment() calculates
a factor, denoted by beta, such that (1 + beta/n) * f(t) provides
finite control of the FDR.

The beta is calculated with the bisection approach. Assume there are beta1
and beta2 such that the choosing beta1 controls the FDR and beta2 not, then the 
optimal beta lies in [beta2, beta1]. If the choice (beta2 + beta1)/2 controls
the FDR, the optimal FDR lies in [(beta2 + beta1)/2, beta1]
and so on.


\code{calculateBetaAdjustment}: The asymptotically optimal rejection curve, denoted by f(t), does not 
provide finite control of the FDR. calculateBetaAdjustment() calculates
a factor, denoted by beta, such that (1 + beta/n) * f(t) provides
finite control of the FDR.

The beta is calculated with the bisection approach. Assume there are beta1
and beta2 such that the choosing beta1 controls the FDR and beta2 not, then the 
optimal beta lies in [beta2, beta1]. If the choice (beta2 + beta1)/2 controls
the FDR, the optimal FDR lies in [(beta2 + beta1)/2, beta1]
and so on.


}
\value{The adjustment factor that is needed to ensure control of
the FDR with the adjusted asymptotically optimal rejection curve
at the specified level and sample size.\code{calculateBetaAdjustment}: The adjustment factor that is needed to ensure control of
the FDR with the adjusted asymptotically optimal rejection curve
at the specified level and sample size.


\code{calculateBetaAdjustment}: The adjustment factor that is needed to ensure control of
the FDR with the adjusted asymptotically optimal rejection curve
at the specified level and sample size.


\code{calculateBetaAdjustment}: The adjustment factor that is needed to ensure control of
the FDR with the adjusted asymptotically optimal rejection curve
at the specified level and sample size.


\code{calculateBetaAdjustment}: The adjustment factor that is needed to ensure control of
the FDR with the adjusted asymptotically optimal rejection curve
at the specified level and sample size.


}
\author{MarselScheer}
\arguments{\item{n}{Number of tests for which the adjusted beta should be calculated.}
\item{startIDX_SUD}{Starting index of the step-up-down procedure}
\item{alpha}{The level at which the FDR shall be controlled.}
\item{silent}{If true any output on the console will be suppressed.}
\item{initialBeta}{}
\item{maxBinarySteps}{}
\item{tolerance}{}}
\alias{calculateBetaAdjustment}

\alias{calculateBetaAdjustment}

\alias{calculateBetaAdjustment}

\alias{calculateBetaAdjustment}


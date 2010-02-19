\name{BR_pi0_est}
\alias{BR_pi0_est}
\title{Estimate of pi0 using the one-step Blanchard-Roquain procedure}
\usage{BR_pi0_est(pValues, alpha, lambda=1, truncate=TRUE)BR_pi0_est(pValues, alpha, lambda=1, truncate=TRUE)
BR_pi0_est(pValues, alpha, lambda=1, truncate=TRUE)
BR_pi0_est(pValues, alpha, lambda=1, truncate=TRUE)
BR_pi0_est(pValues, alpha, lambda=1, truncate=TRUE)
}
\description{The proportion of true nulls is estimated using the Blanchard-Roquain 1-stage 
procedure with parameter (alpha,lambda) via the formula}
\details{estimated pi_0 = ( m - R(alpha,lambda)  + 1) / ( m*( 1 - lambda * alpha ) )

where R(alpha,lambda) is the number of hypotheses rejected by the BR 1-stage procedure,
alpha is the FDR level for this procedure and lambda a 
parameter belonging to (0, 1/alpha) with default value 1. 
Independence of p-values is assumed.
This estimate may in some cases be larger than 1; it is truncated to 1 if the parameter truncated=TRUE.
The estimate is used in the Blanchard-Roquain 2-stage step-up (using the non-truncated version)\code{BR_pi0_est}: estimated pi_0 = ( m - R(alpha,lambda)  + 1) / ( m*( 1 - lambda * alpha ) )

where R(alpha,lambda) is the number of hypotheses rejected by the BR 1-stage procedure,
alpha is the FDR level for this procedure and lambda a 
parameter belonging to (0, 1/alpha) with default value 1. 
Independence of p-values is assumed.
This estimate may in some cases be larger than 1; it is truncated to 1 if the parameter truncated=TRUE.
The estimate is used in the Blanchard-Roquain 2-stage step-up (using the non-truncated version)


\code{BR_pi0_est}: estimated pi_0 = ( m - R(alpha,lambda)  + 1) / ( m*( 1 - lambda * alpha ) )

where R(alpha,lambda) is the number of hypotheses rejected by the BR 1-stage procedure,
alpha is the FDR level for this procedure and lambda a 
parameter belonging to (0, 1/alpha) with default value 1. 
Independence of p-values is assumed.
This estimate may in some cases be larger than 1; it is truncated to 1 if the parameter truncated=TRUE.
The estimate is used in the Blanchard-Roquain 2-stage step-up (using the non-truncated version)


\code{BR_pi0_est}: estimated pi_0 = ( m - R(alpha,lambda)  + 1) / ( m*( 1 - lambda * alpha ) )

where R(alpha,lambda) is the number of hypotheses rejected by the BR 1-stage procedure,
alpha is the FDR level for this procedure and lambda a 
parameter belonging to (0, 1/alpha) with default value 1. 
Independence of p-values is assumed.
This estimate may in some cases be larger than 1; it is truncated to 1 if the parameter truncated=TRUE.
The estimate is used in the Blanchard-Roquain 2-stage step-up (using the non-truncated version)


\code{BR_pi0_est}: estimated pi_0 = ( m - R(alpha,lambda)  + 1) / ( m*( 1 - lambda * alpha ) )

where R(alpha,lambda) is the number of hypotheses rejected by the BR 1-stage procedure,
alpha is the FDR level for this procedure and lambda a 
parameter belonging to (0, 1/alpha) with default value 1. 
Independence of p-values is assumed.
This estimate may in some cases be larger than 1; it is truncated to 1 if the parameter truncated=TRUE.
The estimate is used in the Blanchard-Roquain 2-stage step-up (using the non-truncated version)


}
\value{\item{pi0}{The estimated proportion of true null hypotheses.}\code{BR_pi0_est}: \item{pi0}{The estimated proportion of true null hypotheses.}


\code{BR_pi0_est}: \item{pi0}{The estimated proportion of true null hypotheses.}


\code{BR_pi0_est}: \item{pi0}{The estimated proportion of true null hypotheses.}


\code{BR_pi0_est}: \item{pi0}{The estimated proportion of true null hypotheses.}


}
\author{GillesBlanchard}
\references{BlanchardBlanchardBlanchardBlanchardBlanchard}
\arguments{\item{pValues}{The raw p-values for the marginal test problems (assumed to be independent)}
\item{alpha}{The FDR significance level for the BR procedure}
\item{lambda}{(default 1) The parameter for the BR procedure, shoud belong to (0, 1/alpha)}
\item{truncate}{(logical, default TRUE) if TRUE, output estimated is truncated to 1}}
\alias{BR_pi0_est}

\alias{BR_pi0_est}

\alias{BR_pi0_est}

\alias{BR_pi0_est}


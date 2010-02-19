\name{augmentation}
\alias{augmentation}
\title{Wrapper function to the augmentation methods of the multtest package.}
\usage{augmentation(adjPValues, newErrorControl, newK, newQ, silent=FALSE)augmentation(adjPValues, newErrorControl, newK, newQ, silent=FALSE)
augmentation(adjPValues, newErrorControl, newK, newQ, silent=FALSE)
augmentation(adjPValues, newErrorControl, newK, newQ, silent=FALSE)
augmentation(adjPValues, newErrorControl, newK, newQ, silent=FALSE)
}
\description{Wrapper function to the augmentation methods of the multtest package.}
\details{The augmentation method turns a vector of p-values which are already adjusted for FWER control
into p-values that are adjusted for gFWER, FDX or FDR. The underlying idea (for gFWER and FDX) 
is that the set of hypotheses rejected at a given level alpha under FWER can be "augmented"
by rejecting some additional hypotheses while still ensuring (strong) control of the desired weaker type I criterion.
For FDR, it uses the fact that FDX control for q=alpha=1-sqrt(1-beta) entails FDR control at level beta.

Use of these augmentation methods is recommended only in the situation where FWER-controlled p-values are
directly available from the data (using some specific method). When only marginal p-values are available,
it is generally prerefable to use other adjustment methods directly aimed at the intended criterion
(as opposed to first adjust for FWER, then augment)

Note: in the multtest package, two methods ("restricted" and "conservative") are available for FDR augmentation. 
Here the 'restricted' method is forced for FDR augmentation since it is in fact always valid and better than 
"conservative" (M. van der Laan, personal communication) with respect to power.\code{augmentation}: The augmentation method turns a vector of p-values which are already adjusted for FWER control
into p-values that are adjusted for gFWER, FDX or FDR. The underlying idea (for gFWER and FDX) 
is that the set of hypotheses rejected at a given level alpha under FWER can be "augmented"
by rejecting some additional hypotheses while still ensuring (strong) control of the desired weaker type I criterion.
For FDR, it uses the fact that FDX control for q=alpha=1-sqrt(1-beta) entails FDR control at level beta.

Use of these augmentation methods is recommended only in the situation where FWER-controlled p-values are
directly available from the data (using some specific method). When only marginal p-values are available,
it is generally prerefable to use other adjustment methods directly aimed at the intended criterion
(as opposed to first adjust for FWER, then augment)

Note: in the multtest package, two methods ("restricted" and "conservative") are available for FDR augmentation. 
Here the 'restricted' method is forced for FDR augmentation since it is in fact always valid and better than 
"conservative" (M. van der Laan, personal communication) with respect to power.


\code{augmentation}: The augmentation method turns a vector of p-values which are already adjusted for FWER control
into p-values that are adjusted for gFWER, FDX or FDR. The underlying idea (for gFWER and FDX) 
is that the set of hypotheses rejected at a given level alpha under FWER can be "augmented"
by rejecting some additional hypotheses while still ensuring (strong) control of the desired weaker type I criterion.
For FDR, it uses the fact that FDX control for q=alpha=1-sqrt(1-beta) entails FDR control at level beta.

Use of these augmentation methods is recommended only in the situation where FWER-controlled p-values are
directly available from the data (using some specific method). When only marginal p-values are available,
it is generally prerefable to use other adjustment methods directly aimed at the intended criterion
(as opposed to first adjust for FWER, then augment)

Note: in the multtest package, two methods ("restricted" and "conservative") are available for FDR augmentation. 
Here the 'restricted' method is forced for FDR augmentation since it is in fact always valid and better than 
"conservative" (M. van der Laan, personal communication) with respect to power.


\code{augmentation}: The augmentation method turns a vector of p-values which are already adjusted for FWER control
into p-values that are adjusted for gFWER, FDX or FDR. The underlying idea (for gFWER and FDX) 
is that the set of hypotheses rejected at a given level alpha under FWER can be "augmented"
by rejecting some additional hypotheses while still ensuring (strong) control of the desired weaker type I criterion.
For FDR, it uses the fact that FDX control for q=alpha=1-sqrt(1-beta) entails FDR control at level beta.

Use of these augmentation methods is recommended only in the situation where FWER-controlled p-values are
directly available from the data (using some specific method). When only marginal p-values are available,
it is generally prerefable to use other adjustment methods directly aimed at the intended criterion
(as opposed to first adjust for FWER, then augment)

Note: in the multtest package, two methods ("restricted" and "conservative") are available for FDR augmentation. 
Here the 'restricted' method is forced for FDR augmentation since it is in fact always valid and better than 
"conservative" (M. van der Laan, personal communication) with respect to power.


\code{augmentation}: The augmentation method turns a vector of p-values which are already adjusted for FWER control
into p-values that are adjusted for gFWER, FDX or FDR. The underlying idea (for gFWER and FDX) 
is that the set of hypotheses rejected at a given level alpha under FWER can be "augmented"
by rejecting some additional hypotheses while still ensuring (strong) control of the desired weaker type I criterion.
For FDR, it uses the fact that FDX control for q=alpha=1-sqrt(1-beta) entails FDR control at level beta.

Use of these augmentation methods is recommended only in the situation where FWER-controlled p-values are
directly available from the data (using some specific method). When only marginal p-values are available,
it is generally prerefable to use other adjustment methods directly aimed at the intended criterion
(as opposed to first adjust for FWER, then augment)

Note: in the multtest package, two methods ("restricted" and "conservative") are available for FDR augmentation. 
Here the 'restricted' method is forced for FDR augmentation since it is in fact always valid and better than 
"conservative" (M. van der Laan, personal communication) with respect to power.


}
\value{A list containing:

\item{adjPValues}{A numeric vector containing the new adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected. Currently always NULL.}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function.}\code{augmentation}: A list containing:

\item{adjPValues}{A numeric vector containing the new adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected. Currently always NULL.}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function.}


\code{augmentation}: A list containing:

\item{adjPValues}{A numeric vector containing the new adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected. Currently always NULL.}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function.}


\code{augmentation}: A list containing:

\item{adjPValues}{A numeric vector containing the new adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected. Currently always NULL.}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function.}


\code{augmentation}: A list containing:

\item{adjPValues}{A numeric vector containing the new adjusted pValues}

\item{rejected}{A logical vector indicating which hypotheses are rejected. Currently always NULL.}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function.}


}
\author{KornRohm, GillesBlanchard}
\references{Dudoit, S. and van der Laan, M.J. (2008)
Multiple Testing Procedures with Applications to Genomics, Springer. (chapter 6)Dudoit, S. and van der Laan, M.J. (2008)
Multiple Testing Procedures with Applications to Genomics, Springer. (chapter 6)Dudoit, S. and van der Laan, M.J. (2008)
Multiple Testing Procedures with Applications to Genomics, Springer. (chapter 6)Dudoit, S. and van der Laan, M.J. (2008)
Multiple Testing Procedures with Applications to Genomics, Springer. (chapter 6)Dudoit, S. and van der Laan, M.J. (2008)
Multiple Testing Procedures with Applications to Genomics, Springer. (chapter 6)}
\arguments{\item{adjPValues}{a vector of p-values that are *already* adjusted for FWER}
\item{newErrorControl}{new error control type to adjust for. One of ("gFWER", "FDX", "FDR")}
\item{newK}{k-parameter if newErrorControl is "gFWER"}
\item{newQ}{q-parameter if newErrorControl is "FDX"}
\item{silent}{if true any output on the console will be suppressed.}}
\examples{\dontrun{ TODO NH (MS) EXAMPLE PROBLEM
p <- c(runif(50), runif(50, 0, 0.01))
fwer_adj <- bonferroni(p)    # adjust for FWER using Bonferroni correction
fdx_adj  <- augmentation(fwer_adj$adjPValues , "FDX", q=0.1, silent = TRUE) #augment to FDX (q=0.1)
}\dontrun{ TODO NH (MS) EXAMPLE PROBLEM
p <- c(runif(50), runif(50, 0, 0.01))
fwer_adj <- bonferroni(p)    # adjust for FWER using Bonferroni correction
fdx_adj  <- augmentation(fwer_adj$adjPValues , "FDX", q=0.1, silent = TRUE) #augment to FDX (q=0.1)
}\dontrun{ TODO NH (MS) EXAMPLE PROBLEM
p <- c(runif(50), runif(50, 0, 0.01))
fwer_adj <- bonferroni(p)    # adjust for FWER using Bonferroni correction
fdx_adj  <- augmentation(fwer_adj$adjPValues , "FDX", q=0.1, silent = TRUE) #augment to FDX (q=0.1)
}\dontrun{ TODO NH (MS) EXAMPLE PROBLEM
p <- c(runif(50), runif(50, 0, 0.01))
fwer_adj <- bonferroni(p)    # adjust for FWER using Bonferroni correction
fdx_adj  <- augmentation(fwer_adj$adjPValues , "FDX", q=0.1, silent = TRUE) #augment to FDX (q=0.1)
}\dontrun{ TODO NH (MS) EXAMPLE PROBLEM
p <- c(runif(50), runif(50, 0, 0.01))
fwer_adj <- bonferroni(p)    # adjust for FWER using Bonferroni correction
fdx_adj  <- augmentation(fwer_adj$adjPValues , "FDX", q=0.1, silent = TRUE) #augment to FDX (q=0.1)
}}
\alias{augmentation}

\alias{augmentation}

\alias{augmentation}

\alias{augmentation}


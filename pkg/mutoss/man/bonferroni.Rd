\name{bonferroni}
\alias{bonferroni}
\title{Bonferroni correction...}
\usage{bonferroni(pValues, alpha, silent=FALSE)}
\arguments{
  \item{pValues}{
	A numeric vector containing the unadjusted pValues.
  }
  \item{alpha}{
	Overall type I error rate alpha.
  }
  \item{silent}{
  	logical scalar. If \code{TRUE} no output is generated.  
  }
}
\description{Bonferroni correction}
\details{The classical Bonferroni correction outputs adjusted p-values, ensuring strong FWER control under arbitrary
dependence of the input p-values. It simply multiplies each input p-value by the total number of hypotheses
(and ceils at value 1).

It is recommended to use Holm's step-down instead, which is valid under the exact same assumptions and more powerful.}
\value{A list containing:

\item{adjPValues}{A numeric vector containing the new adjusted pValues}

\item{rejected}{(if alpha is given) A logical vector indicating which hypotheses are rejected}

\item{errorControl}{A Mutoss S4 class of type \code{errorControl}, containing the type of error controlled by the function.}}
\references{Bonferroni, C. E. (1935) Il calcolo delle assicurazioni su gruppi di teste. 
'In Studi in Onore del Professore Salvatore Ortu Carboni. Rome: Italy, pp. 13-60.

Bonferroni, C. E. (1936) Teoria statistica delle classi e calcolo delle probabilita.
Pubblicazioni del R Istituto Superiore di Scienze Economiche e Commerciali di Firenze 8, 3-62, 1936.}
\author{KornRohm}
\arguments{\item{pValues}{pValues to be used. No assumption is made on the dependence structure.}
\item{alpha}{The level at which the FWER shall be controlled (optional).}}

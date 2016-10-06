# mutoss GITHUB README
Mutoss (multiple hypotheses testing in an open software system) aims at providing a unified, extensible interface covering a large spectrum of multiple hypotheses testing procedures in R. Features a GUI and a simulation tool. Funded by PASCAL2.

![GUI screenshot](https://raw.github.com/kornl/mutoss/master/build/GUI.png)

The development version of this package can be directly installed with the R package devtools from Hadley Wickham et al. using:

    install.packages("devtools")
    library(devtools)
    install_github("kornl/mutoss", subdir="pkg/mutoss", dependencies = TRUE, build_vignettes = TRUE)
    install_github("kornl/mutoss", subdir="pkg/mutossGUI", dependencies = TRUE, build_vignettes = TRUE)

(If some LaTeX packages are missing, consider `build_vignettes = FALSE` or install them.)

But note that the mutossGUI jar file is only rarely updated and it may be appropriate to build it yourself from the Java source code.

Otherwise just use the current version from [CRAN](http://cran.r-project.org/web/packages/mutoss/).

#In R exist:
#===========

?p.adjust()
# in base, methods = c("holm", "hochberg", "hommel", 
#                      "bonferroni", "BH", "BY",
#                      "fdr", "none")


?TukeyHSD()
# SimCI, adj-p.Values

library(multcomp)
# arbitrary contrasts, Dunnett, SimCI, adj-p.Values

library(nparcomp)
# nonparametric, SimCI, adj-p.Values

source("http://bioconductor.org/biocLite.R")
biocLite("Biobase")
install.packages("multtest")
# Bootstrap, resampling, maxT, ...

library(car)
?confidence.ellipse(Scheffe=TRUE)

# Missing: Tarone, SNK, Royen-Shaffer, Sidak, GT2, REGWQ, Fallback, Rom's, Rank trunked.
#
#       ,-"""-.
#  oo._/ \___/ \
# (____)_/___\__\_)    Schildkroete-Code will be submitted!
#     /_//   \\_\

# What about a framework for closed test procedures? Does this makes sense?

library(AGSDest)
# Estimation in adaptive group sequential trials

library(gsDesign)
# gsDesign is a package that derives group sequential designs and describes their properties.
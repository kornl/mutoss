# TODO: Add comment
# 
# Author: MarselScheer
###############################################################################


require(XML)
require(mutoss)
sink("page.html")
cat("<h3>output</h3><ul>", mutoss.BH()@output, "</ul> <ul> lksjdf </ul>")
cat(mutoss.BH()@info)
sink()
h <- list(file="~/page.html")
class(h) <- "html"




mutossGUI <- function(debugOutput=FALSE){
  if (!startGUI()) return(invisible(NULL))
	#gui <- .jnew("mutoss.gui.MuTossGUI")
	.jcall("org/mutoss/gui/MuTossGUI", returnSig = "V", method="startGUI", debugOutput)
}

reportBug <- function(){
	.jcall("org/mutoss/gui/MuTossGUI", method="reportBug")
}


startGUI <- function() {
  if (!"jri.jar" %in% tolower(sapply(.jclassPath(), function(x) {substring(x, first=nchar(x)-6)}))) {
    warning(paste(c("JRI.jar seems to be missing from the classpath. ",
                    "The graphical user interface will most likely not be available. ",
                    "Compile R with shared library enabled (--enable-R-shlib option) ",
                    "and reinstall rJava to use JRI functionality."), collapse="\n"))
  }
  
  if ("tools:rstudio" %in% search()) {
    if (interactive()) {
      cat("Starting the graphical user interface from within RStudio may crash. \nPlease use R without RStudio for the GUI (all the other command line functions are fine).")
      line <- "?"
      while (!(tolower(line) %in% c("y","n") )) {
        line <- readline("Do you want to start the GUI nevertheless? (y/n) ")
      }
      if (tolower(line)=="n") {
        return(FALSE)
      }
    } else {
      warning("Starting the graphical user interface from within RStudio may crash. \nPlease use R without RStudio for the GUI (all the other command line functions are fine).")
    }
  }
  return(TRUE)
}

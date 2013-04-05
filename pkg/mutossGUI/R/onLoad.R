.onLoad <- function(libname, pkgname) {
	.jinit(parameters="-Xrs")
	.jpackage(pkgname)
	
	classes <- system.file("jri", package = "rJava", lib.loc = NULL)
	if (nchar(classes)) {
		.jaddClassPath(classes)
		jars <- grep(".*\\.jar", list.files(classes, full.names = TRUE), TRUE, value = TRUE)
		if (length(jars)) { 
			.jaddClassPath(jars)
		}		
	}
	
	rJavaVersion <- utils::sessionInfo()$otherPkgs$rJava$Version
	# If we have a rJava version > 0.9-3 load JRIEngine.jar and REngine.jar
    if (!is.null(rJavaVersion) && rJavaVersion > "0.9-3") {
		classes <- system.file("JRI", package = "CommonJavaJars", lib.loc = NULL)
		if (nzchar(classes)) {
			.jaddClassPath(classes)
			jars <- grep(".*\\.jar", list.files(classes, full.names = TRUE), TRUE, value = TRUE)
			if (length(jars)) { 
				.jaddClassPath(jars)
			}		
		}
	}
	
	classes <- system.file("java", package = "JavaGD", lib.loc = NULL)
	if (nchar(classes)) {
		.jaddClassPath(classes)
		jars <- grep(".*\\.jar", list.files(classes, full.names = TRUE), TRUE, value = TRUE)
		if (length(jars)) { 
			.jaddClassPath(jars)
		}		
	}
	
	jars <- c("afcommons", "commons-collections", "commons-lang", 
			"commons-logging", "commons-validator", "forms", 
			"iText", "jhlir.jar", "jxlayer", 
			"log4j", "swing-worker", "poi")
	
	loadJars(jars)
	
	## we supply our own JavaGD class
	Sys.setenv("JAVAGD_CLASS_NAME"="org/mutoss/gui/JavaGD")
} 

.onAttach <- function(libname, pkgname) {
	packageStartupMessage("\nFor starting the MuToss-GUI enter:\nmutossGUI()\n")
}
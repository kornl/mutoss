.onLoad <- function(libname, pkgname) {
  .jinit(parameters=c("-Xrs", "-Xss1m",
                      paste0("-Djava.io.tmpdir=", tempdir())))
	.jpackage(pkgname)
	.jpackage("JGR")
	
	classes <- system.file("jri", package = "rJava", lib.loc = NULL)
	if (nchar(classes)) {
		.jaddClassPath(classes)
		jars <- grep(".*\\.jar", list.files(classes, full.names = TRUE), TRUE, value = TRUE)
		if (length(jars)) { 
			.jaddClassPath(jars)
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
			"iText", "jhlir.jar", "jxlayer", "log4j-1.2-api", "log4j-api", "log4j-core", "swing-worker")
	
	loadJars(jars)
  # As soon as we require CommonJavaJars >= 1.0-5 we can drop this following line:
  try(loadJars("jgoodies-common"), silent=TRUE)
	
	## we supply our own JavaGD class
	Sys.setenv("JAVAGD_CLASS_NAME"="org/mutoss/gui/JavaGD")
} 

.onAttach <- function(libname, pkgname) {
	packageStartupMessage("\nFor starting the MuToss-GUI enter:\nmutossGUI()\n")
}
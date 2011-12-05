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
	
	classes <- system.file("java", package = "JavaGD", lib.loc = NULL)
	if (nchar(classes)) {
		.jaddClassPath(classes)
		jars <- grep(".*\\.jar", list.files(classes, full.names = TRUE), TRUE, value = TRUE)
		if (length(jars)) { 
			.jaddClassPath(jars)
		}		
	}
	
	classes <- system.file("java", package = "CommonJavaJars", lib.loc = NULL)
	if (nchar(classes)) {
		.jaddClassPath(classes)
		jars <- grep(".*\\.jar", list.files(classes, full.names = TRUE), TRUE, value = TRUE)
		if (length(jars)) { 
			.jaddClassPath(jars)
		}		
	}
	
	## we supply our own JavaGD class
	.setenv <- if (exists("Sys.setenv")) Sys.setenv else Sys.putenv
	.setenv("JAVAGD_CLASS_NAME"="org/mutoss/gui/JavaGD")  
	require(mutoss)
	packageStartupMessage("\nFor starting the MuToss-GUI enter:\nmutossGUI()\n")
}  
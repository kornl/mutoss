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
	
	jarsSC <- c("afcommons", "commons-collections", "commons-lang", 
			"commons-logging", "commons-validator", "forms", 
			"iText", "jhlir.jar", "jxlayer", 
			"log4j", "mysql-connector-java", 
			"swing-worker", "poi")
	
	jars <- c()	
	classes <- system.file("java", package = "CommonJavaJars", lib.loc = NULL)
	files <- list.files(classes, full.names = FALSE)
	# For now always ignore the jars that require Java >= 6.
	files <- grep("J6", files, TRUE, value = TRUE, invert = TRUE)
	for (j in jarsSC) {
		# Always take the newest jar per default:
		jars <- c(jars, sort(grep(j, files, TRUE, value = TRUE), decreasing = TRUE)[1])
	}
	
	.jpackage("CommonJavaJars", jars=jars)
	
	## we supply our own JavaGD class
	Sys.setenv("JAVAGD_CLASS_NAME"="org/mutoss/gui/JavaGD")
	packageStartupMessage("\nFor starting the MuToss-GUI enter:\nmutossGUI()\n")
}  
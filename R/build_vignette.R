#' Hack to build the vignettes and make them available in devtools::install_github()
#' Found on https://community.rstudio.com/t/browsevignettes-mypackage-saying-no-vignettes-found/68656/6
#' @name buildVignette
#' @docType methods
#' @description Build Vignettes and make them available
#' @param dir the path of the object's dev folder
#' @title helper to prepare the package for github
#' @export 
buildVignette <- function(dir =".") {
	dir = normalizePath(dir)
	tools::buildVignettes(dir = dir, tangle=TRUE)
	if ( ! file.exists( file.path(dir,"inst","doc")) ){
		dir.create(file.path(dir,"inst","doc"))
	}
	file.copy(dir("vignettes", full.names=TRUE), file.path(dir,"inst","doc"), overwrite=TRUE)


	## now let's update the portable log, too
	knitr::purl(file.path(dir, 'vignettes','cellexalvrR-linearSelections-vignette.Rmd'))
	tmpDir = tempdir()
	origDir = getwd()
	setwd(tmpDir)
	tryCatch( {
		source( file.path(dir, 'vignettes','cellexalvrR-linearSelections-vignette.R') )
		## now we should have a tmpDir/PortableLog_linearExample.zip that we need to copy to the inst/extdata folder
		if ( file.exists( file.path( tmpDir, 'PortableLog_linearExample.zip'))){
			file.copy( file.path( tmpDir, 'PortableLog_linearExample.zip'), file.path( dir, 'inst', 'extdata') )
		}else {
			warning( "The expected portable log file could not be copied!")
			browser()
		}
		}, error(err) { 
			setwd( origDir )
			error(err)
		}
	)
}
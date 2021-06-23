#' Hack to build the vignettes and make them available to devtools::install_github()
#' Found this fix on https://community.rstudio.com/t/browsevignettes-mypackage-saying-no-vignettes-found/68656/6
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
		new= file.path( tmpDir, 'PortableLog_linearExample.zip')
		save = file.path( dir, 'inst', 'extdata','PortableLog_linearExample.zip')
		if ( file.exists( new )){
			if ( file.exists( save )){
				unlink(save)
			}
			file.copy( new, save )
		}else {
			warning( "The expected portable log file could not be copied!")
			browser()
		}
		}, error=function(err) { 
			setwd( origDir )
			error(err)
		}
	)
	setwd( origDir )
	return(1)
}
context('re-create Vignettes')


prefix = file.path(".","inst", "doc")
savePath = file.path('.',"helperScripts","vignettes")

wd = getwd()
setwd( file.path('..', '..')) ## if a normal test run - skip this if running manually.

buildVignette()

vignetteFiles = c('cellexalvrR-vignette.html', 'cellexalvrR-linearSelections-vignette.html', 'cellexalvrRQuickstart.html')

for (v in vignetteFiles){

	expect_true( file.exists(file.path(prefix, v)), 
		label=paste("vignette file", v,"in inst/doc" ) )
	expect_true( file.exists(file.path(savePath, v)) , 
		label=paste("vignette file", v,"in helperScripts/vignettes" ) )
}

setwd(wd)
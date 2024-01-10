context('new selection location impl')


if ( ! expect_true( rmarkdown::pandoc_available() ,label= "pandoc is installed") ){
	skip ( "Pandoc needed - but missing here")
}
Sys.setenv(TEST_MODE = "TRUE")

prefix = './'


if ( file.exists( file.path(prefix, 'function.R'))){
	## the test functions that should not be maintained in differet test scripts...
	source( file.path(prefix, 'function.R')) 
}

if ( file.exists( file.path(prefix,'data','output','integrationTest1' ) )){
	unlink(  file.path(prefix,'data','output','integrationTest1' ), recursive=TRUE )
}

x = reset(cellexalObj)
x@outpath = file.path(prefix,'data','output','integrationTest1' )

x = sessionPath( x, 'NewSelectionLocation' )

dir.create( file.path( x@outpath, 'Selections','Selection0'), recursive=TRUE )

grouping <- file.path(prefix, 'data/SelectionHSPC_time.txt')

selectionF = file.path(  x@outpath, 'Selections','Selection0', 'selection.txt' )
file.copy( grouping, selectionF ) 

make.cellexalvr.heatmap.list( x, selectionF, 250, file.path( x@outpath, 'Heatmaps', 'ThisIntegrattionTestResult_0815') )

x = renderReport(x)
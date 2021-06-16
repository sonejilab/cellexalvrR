context('function test simpleHeatmaps function')

#prefix = 'tests/testthat'
prefix = '.'

#print(file.path(getwd(), prefix, 'data', 'simpleHeatmap_mat.RData' ) )

#cellexalObj <- loadObject(file.path(prefix,'data','cellexalObjOK.RData') )

x = cellexalObj

x = reset(x)

x@outpath = file.path(prefix,'data','output','simpleHeatmaps' )
if ( file.exists(x@outpath ) ){
	unlink( x@outpath ,recursive=TRUE)
}
dir.create( x@outpath )

x = sessionPath(x, 'simpleHeatmap' )


grouping <- normalizePath (file.path(prefix, 'data', 'SelectionHSPC_time.txt' ))
x = userGrouping( x, grouping)
x = pseudotimeTest3D( x, grouping = x@usedObj$lastGroup )

x = createStats( x@usedObj$timelines[[1]], x)

timeline = x@usedObj$timelines[[1]]

##that is the first time we produce a time in a test script
## Time to test this!
expect_equal( timeline@gname, 'Time.group.2', "time gname is correct" )
expect_equal( timeline@parentSelection, 'User.group.1', "time parentSelection is correct" )
expect_equal( timeline@geneClusters, list(), label="geneClsuters are not populated" )
expect_equal( timeline@id, "7e508e3670c18c3438feeddc8e793ebe", label="id correct" )
expect_equal( timeline@drc, "DDRtree", label="drc correct" )
expect_equal( length(timeline@error), 0 , label="no error" )


fname= file.path(x@usedObj$sessionPath,'png', 'simpleHeatmap' )

res = simplePlotHeatmaps (x, info = groupingInfo( x, timeline@gname), fname )
expect_equal( names(res), c("genes", "ofile", "pngs", "groupColors", "error",
 'smoothedClusters', 'MaxInCluster', "mat" ) )
expect_equal( length(res$genes), 6, label="6 gene groups")
expect_equal( res$ofile , paste(sep=".", fname, 'png'), label="ofile correct" )
expect_equal( length(res$pngs), 6, label="6 heatmap pngs")


for ( f in c( res$ofile, (res$pngs )) ){
	expect_true( file.exists( f ), label=f )
}

expect_equal( res$error, NULL, label="no error" )

expect_equal( dim(res$mat), c( 159, 250 ), label="zscored dimension OK" )



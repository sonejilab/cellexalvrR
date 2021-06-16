context('Networks')
prefix = '.'
opath = file.path(prefix, 'data','output','02networks')
ipath = file.path(prefix, 'data')
ofiles =c( 'Networks.nwk', 'NwkCentroids.cnt' )

if ( file.exists( opath) ){
	unlink( opath, recursive=TRUE)
}
dir.create( opath )

#load(system.file( 'data/cellexalObj.rda', package='cellexalvrR'))

cellexalObj = check(cellexalObj)
cellexalObj@outpath = opath
lockedSave( cellexalObj )

cellexalObj = useInbuiltGOIlists(cellexalObj, 'TFs')

cellexalObj = check(cellexalObj)

make.cellexalvr.network ( file.path(opath, 'cellexalObj.RData') , file.path(ipath, 'selection0.txt'), opath )

for ( f in ofiles ) {
	ofile = file.path(opath, f )

	#print( ofile )
	expect_true( file.exists( ofile ), paste("outfile exists", ofile) )
}

## now add the log network test hereprefix = './'

## the network image is created by the VR process - hence I need a dummy here!

genes = rownames(cellexalObj@data)[1:210]
if ( ! file.exists(file.path(opath, 'tmp') )){
	dir.create( file.path(opath, 'tmp') )
}
grDevices::png( file=file.path(opath, 'tmp', 'a_simple_figure2.png'), width=800, height=800 )
plot(1:100, sample(100:1, 100), main="Just for the test 1!" )
grDevices::dev.off()

heatmap_png <- file.path(opath, 'tmp', 'a_simple_figure2.png')

cellexalObj = sessionPath (cellexalObj, 'LogNetworkTest' )

ofile = file.path( cellexalObj@outpath, 'AB_Network_LogNetworkTest.html')

if ( file.exists( ofile ) ) {
	unlink( ofile )
}
cellexalObj = logNetwork(cellexalObj, genes, heatmap_png, file.path(ipath, 'selection0.txt') )

expect_true( file.exists( ofile ), paste("outfile exists", ofile) )
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
if ( file.exists( file.path(opath, 'cellexalObj.RData'))){
	unlink( file.path(opath, 'cellexalObj.RData') )
}

lockedSave( cellexalObj )

cellexalObj = useInbuiltGOIlists(cellexalObj, 'TFs')

cellexalObj = check(cellexalObj)

expect_true( file.exists( file.path(opath, 'cellexalObj.RData')), label="expected cellexal file exists")

cellexalObj = reset( cellexalObj )
cellexalObj = sessionPath( cellexalObj, 'logNetworkTest')

make.cellexalvr.network ( cellexalObj , file.path(ipath, 'selection0.txt'), opath )

#cellexalObj = renderReport(cellexalObj)
#skip("First things first")

for ( f in ofiles ) {
	ofile = file.path(opath, f )

	#print( ofile )
	expect_true( file.exists( ofile ), paste("outfile exists", ofile) )
}

## now add the log network test here

## the network image is created by the VR process - hence I need a dummy here!

genes = rownames(cellexalObj@data)[1:210]
if ( ! file.exists(file.path(opath, 'tmp') )){
	dir.create( file.path(opath, 'tmp') )
}
grDevices::png( file=file.path(opath, 'tmp', 'a_simple_figure2.png'), width=800, height=800 )
plot(1:100, sample(100:1, 100), main="Just for the test 1!" )
grDevices::dev.off()

heatmap_png <- file.path(opath, 'tmp', 'a_simple_figure2.png')

#cellexalObj = sessionPath (cellexalObj, 'logNetworkTest' )

ofile = file.path( cellexalObj@outpath, 'AC_Network_logNetworkTest.html')

if ( file.exists( ofile ) ) {
	unlink( ofile )
}
cellexalObj = logNetwork(cellexalObj, genes, heatmap_png, file.path(ipath, 'selection0.txt') )

expect_true( file.exists( ofile ), label= paste("outfile exists", ofile) )

cellexalObj = renderReport(cellexalObj)

ofiles = c(  "cellexalObj.RData", "libs", "logNetworkTest", "Networks.nwk", "NwkCentroids.cnt", 
	"PortableLog_logNetworkTest.zip", "reference-keys.txt", "search_index.json"
	, "session-log-for-session-logNetworktest.html", "tmp")

for ( f in ofiles){
	expect_true( file.exists( file.path(cellexalObj@outpath, f)), label =paste("file", f))
}
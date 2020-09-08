context('Networks')
prefix = '.'
opath = file.path(prefix, 'data','output')

ofiles =c( 'Networks.nwk', 'NwkCentroids.cnt' )

for ( f in ofiles ) {
	ofile = file.path(opath, f ) 
	if(  file.exists(ofile ) ){
		unlink( ofile)
	}
}
#cellexalObj = loadObject( file.path(opath, 'cellexalObj.RData') )
cellexalObj = useInbuiltGOIlists(cellexalObj, 'TFs')
cellexalObj@outpath = opath
#browser()
make.cellexalvr.network ( cellexalObj  , file.path(opath,'..', 'selection0.txt'), opath )

for ( f in ofiles ) {
	ofile = file.path(opath, f )
	print( ofile )
	expect_true( file.exists( ofile ), paste("outfile exists", ofile) )
}



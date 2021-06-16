context('myNextCoolHeatmapFeature')

prefix= './'
#prefix = 'tests/testthat'

path = file.path( prefix,'data', 'output', 'myNextCollHeatmapFeature')
if ( file.exists( path)) {
	unlink(path, recursive=TRUE)
}
dir.create(path)


cellexalObj@outpath = path

selection = file.path( prefix,'data', 'selection11.txt')
outfile =  file.path( cellexalObj@outpath, 'finished.txt' )

expect_true( file.exists( selection ), label="selection file exists" )
expect_true( ! file.exists( outfile), label="The outfile is not there before function run" )

cellexalObj = myNextCoolHeatmapFeature( cellexalObj, selection, outfile)

expect_true(  file.exists( outfile), label="The outfile exists after function run" )

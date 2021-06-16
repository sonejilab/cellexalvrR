context('server Simple - screenshots')

prefix= './'
#prefix = 'tests/testthat'

path = file.path( prefix,'data', 'output')

oldF = c()

new = file.path( path, 'test.png')
if ( file.exists(new)){
	unlink(new)
}
oldF = newScreenshots( oldF, path )
newF = newScreenshots( oldF, path )
expect_true( length(newF) == 0 ,"no new files" )
file.create( file.path( path, 'test.png'))
newF = newScreenshots( oldF, path )
expect_true( length(newF) == 1 ,"one new file" )
expect_true( newF == 'test.png' ,"one new file" )


context('server SIMPLE')

skip( "only activate if you know what you do here!" )

if ( is.na( match('cellexalvrR',rownames(installed.packages()))) ) {
	skip("cellexalvrR has to be installed before this test")
}else if ( installed.packages()['cellexalvrR','Version'] != packageDescription("cellexalvrR")$Version) {
	print ( "Please re-run this test with the updated cellexalvrR lib installed if any error occures" )
}

tmpFile = tempfile(pattern = "file", tmpdir = tempdir(), fileext = "")

srvFile =  paste( tmpFile, 'serverR', sep='.')

pidfile    = paste( tmpFile, 'pid', sep='.')
scriptfile = paste( tmpFile, 'input.R', sep=".")
lockfile   = paste( tmpFile, 'input.lock', sep=".") 
pv_file    = paste( tmpFile, 'cellexalvrR.version', sep='.')


message( tmpFile )
warning( paste("please remove the Server.R file by hand! LOCKING!! ", tmpFile))


prefix = '.'
ipath = file.path( prefix, 'data' )


server( srvFile, debug=F, sleep=1, asFunction=T )


expect_true( TRUE ,"the server shut down without a hassle")
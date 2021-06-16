context('create sessionPath')

prefix = './'
#prefix = 'tests/testthat'

#data = file.path(prefix, 'data/cellexalObj.RData')

#cellexalObj = loadObject( data )
cellexalObj = reset(cellexalObj)
datadir = file.path( prefix, 'data', 'output','sessionPath')

if ( file.exists( datadir) ) {
	unlink( datadir, recursive=TRUE )
}
dir.create( datadir )

datadir <- normalizePath(datadir)

cellexalObj@outpath = datadir ## to not mess up the package

pidfile = file.path(cellexalObj@outpath, 'mainServer.pid')

if ( file.exists(pidfile)){
	unlink( pidfile )
}

cellexalObj = sessionPath( cellexalObj )

#"2020_09_30_09_17_08"
seped = as.numeric(unlist(stringr::str_split (cellexalObj@usedObj$sessionName,"_")))
expect_true( length(seped) == 6)
expect_true( all( is.numeric(seped)))

expect_true( file.exists(cellexalObj@usedObj$sessionPath ), 
	label="session path created")

for ( f in c('png', 'tables') ) {
	expect_true( file.exists(file.path(cellexalObj@usedObj$sessionPath,f) ), label=paste("session sub-path",f) )

}

defaultW <- getOption("warn")
options(warn = -1)
Sys.sleep(1) ## to make the timestamp different.
## this should not be overwritable without a renderReport!
old= cellexalObj@usedObj$sessionName
cellexalObj = sessionPath( cellexalObj, 'somethingNew' )
expect_true( cellexalObj@usedObj$sessionName == old, label="session name is not changable in session")

old= cellexalObj@usedObj$sessionName
cellexalObj = sessionPath( cellexalObj, 'somethingNew' )
expect_true( cellexalObj@usedObj$sessionName == old, label="session name is really not changable in session")
options(warn = defaultW)

context('create sessionPath - simulated server')

cellexalObj@usedObj$sessionPath = cellexalObj@usedObj$sessionRmdFiles = cellexalObj@usedObj$sessionName = NULL


cat( Sys.getpid() , file = pidfile )

cellexalObj = sessionPath( cellexalObj, old )

expect_true(file.exists( file.path(cellexalObj@outpath, 'mainServer.sessionName')), label='file mainServer.sessionName')

cellexalObj@usedObj$sessionPath = cellexalObj@usedObj$sessionRmdFiles = cellexalObj@usedObj$sessionName = NULL

cellexalObj = sessionPath( cellexalObj, 'something' )

expect_true(cellexalObj@usedObj$sessionName ==  old, label=paste("session name is read from file old =",old, "== new =", cellexalObj@usedObj$sessionName," ?") )

## so if we start from scratch here and reset the obejct.
## I still want it to have the same session name here!

cellexalObj = reset(cellexalObj)

expect_true( ! file.exists(file.path(cellexalObj@outpath, 'mainServer.sessionName')),
 label="reset removes sessionName file" )

cellexalObj = sessionPath( cellexalObj )

expect_true( cellexalObj@usedObj$sessionName != old, 
	label=paste("is not read from sesssionName file", 
		cellexalObj@usedObj$sessionName," != ",old) )

cellexalObj = reset(cellexalObj)

# writeLines( "shoulNotBeRead" ,  file.path(cellexalObj@outpath, 'mainServer.sessionName') )

# expect_true( cellexalObj@usedObj$sessionName != "shoulNotBeRead",
# 	label="sessionName file is ignored without pid file")

unlink( pidfile )
unlink( file.path(cellexalObj@outpath, 'mainServer.sessionName') )


cellexalObj = sessionPath( cellexalObj, 'newSession' )

expect_true( cellexalObj@usedObj$sessionName == 'newSession', label="without server session the session can be reset.")

#expect_true( ! file.exists(file.path(cellexalObj@outpath, 'mainServer.sessionName')), 
#	label="sessionName is not create if not in server mode" )

cellexalObj= renderReport(cellexalObj)

expect_true( file.exists( file.path(cellexalObj@outpath, 'session-log-for-session-newsession.html')), label="final report is created")

expect_true( ! file.exists(file.path(cellexalObj@outpath, 'mainServer.sessionName')), label="renderReport removes sessionName file" )


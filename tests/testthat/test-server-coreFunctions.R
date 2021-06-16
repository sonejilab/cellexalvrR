context('server offline testing')

if ( .Platform$OS.type != 'unix' ) {
	skip('Test only work on unix platform')
}

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


prefix = '.'
ipath = file.path( prefix, 'data' )

## This function is crucial as it checks that the R script is finished using file.exists(scriptfile)
## and waits. For a working version should it also restart the server after some time?
write_lines <- function( x, f=scriptfile, maxWait=5) {
	if ( maxWait > 0 ) {
	  wait4server ( f, maxWait )
	}
	file.create(lockfile)
	
	fileConn<-file(f)
	writeLines( x , fileConn)
	close( fileConn )
	file.remove(lockfile)
	
	invisible(NULL)
}

wait4server<- function( file=scriptfile,  maxWait=15, pidTest=TRUE) {
	while ( file.exists(file) ){
		Sys.sleep( 1 )
		maxWait = maxWait -1;
		if(maxWait == 0)
			break;
	}
	if ( file.exists(file) ){
		stop( "Server is not clear for usage - start a new instance?")
	}
	if ( pidTest ){
	if( ! isAlive(pid) ) {
		stop("Server crashed!")
	}}
}
	
isAlive <- function( pid ) {
	tools::pskill( pid, 0) == T
}


## this will become the master we work with
write_lines( c( 
	"library(cellexalvrR)", 
	paste(sep="","server( file='",tmpFile,"', debug=TRUE )" ) ), 
	f= srvFile , 0
)



#startCMD = paste( "paste(", R.exe(),",'CMD BATCH',", file2Script(srvFile),",'&')" )

startCMD = paste( R.exe(),"CMD BATCH", srvFile,"&" )
#print (startCMD)

cat('Sys.sleep(10)', file=scriptfile)
expect_true(file.exists( scriptfile))

system( startCMD )

Sys.sleep(10)

#system( paste( file.path(R.home("bin"), "R CMD BATCH") , srvFile , " &") )

wait4server ( scriptfile, pidTest=FALSE )

expect_true(!file.exists( scriptfile))

expect_true(file.exists( pidfile ))

pid = scan( pidfile, quiet=TRUE )
#print ( paste("The PID of the server is", pid ))
expect_true(isAlive(pid))

expect_true(file.exists( pv_file ))
vers = scan( pv_file, what=character() )

#print ( paste( vers, '==', packageVersion("cellexalvrR") ))
expect_true(vers == packageVersion("cellexalvrR") )

## I can re-use the server file

context('starting server in slave mode')

srvFile2 =  paste( tmpFile,'slave', 'serverR', sep='.')


tmpFile2 = paste(tmpFile,"2", sep="")
write_lines( c( 
	"library(cellexalvrR)", 
	paste(sep="","server( file='",tmpFile2,"', debug=TRUE, masterPID=",pid," )" ) ), 
	f= srvFile2 , 0
)

startCMD2 = paste( R.exe(),"CMD BATCH", srvFile2,"&" )

#print ( "starting slave server" )
system( startCMD2 )

Sys.sleep(10)

slavePID = scan( paste( tmpFile2, 'pid', sep='.' ), quiet=TRUE)

expect_true(isAlive(slavePID), "slave failed to start up")


## I can re-use the server file

context('starting server in slave mode')

tmpFile2 = paste(tmpFile,"2", sep="")
write_lines( c( 
	"library(cellexalvrR)", 
	paste(sep="","server( file='",tmpFile2,"', debug=TRUE, masterPID=",pid," )" ) ), 
	f= srvFile , 0
)

#print ( "starting slave server" )
system( startCMD )

Sys.sleep(10)

slavePID = scan( paste( tmpFile2, 'pid', sep='.' ), quiet=TRUE)

expect_true(isAlive(slavePID), "slave failed to start up")




## would only work on linux....
#system(paste( sep="",'ps -af | grep "',pid,'"  | grep -v grep '))

context('server create a png')

write_lines( c(
paste(sep="", "png(file=paste( sep='.',",file2Script(tmpFile),",'png'), width=800, height=800)"),
"plot ( 1:10, 1:10)",
"dev.off()"))

while( file.exists(scriptfile )){
	Sys.sleep( 1 )
}

expect_true(isAlive(pid))


expect_true(file.exists( paste(tmpFile, 'png', sep='.' )))

context('server does not crash using useless blaberish')

write_lines(  paste(sep="", "This will bvbreake horribly") )

expect_true(isAlive(pid))

write_lines(  paste(sep="", "stop('This will bvbreake horribly')") )

expect_true(isAlive(pid))

context('server make.cellexalvr.heatmap.list')

expect_true(isAlive(pid))

## so now lets try some real things
## we have a inbuild dataset ;-)
## Hence we can do funny things like get a list of differential genes
write_lines( paste( "cellexalObj@outpath='",tmpFile,"'", sep="") )
dir.create( tmpFile )

#system(paste( sep="",'ps -af | grep "',pid,'"  | grep -v grep '))
wait4server()


write_lines(c(" ", paste( sep="",
	"make.cellexalvr.heatmap.list(cellexalObj, ",file2Script(file.path(ipath,'selection0.txt')),", 250, ",file2Script( paste(sep=".", tmpFile, "diffGenes")),", 'wilcox' )" 
)	) )


wait4server()
expect_true(!file.exists( scriptfile))

expect_true(isAlive(pid))

expect_true(file.exists( paste(tmpFile, 'diffGenes', sep='.' )))
diffGenes = scan( paste(tmpFile, 'diffGenes', sep='.' ), what=character())[-1]

message(paste( "number of  diff genes", length(diffGenes),"expected 252" ) )
expect_true( length(diffGenes) == 252)


context('server make.cellexalvr.network')

## that does not work - the networks need different input?
write_lines( paste( sep="",
	"make.cellexalvr.network(cellexalObj, 'GMP_broad', ",file2Script(paste(sep=".", tmpFile, "Networks")),", 'wilcox' )" 
)	)
## but the block in the write_lines works fine!

context('server get.genes.cor.to')

write_lines( paste( sep="", "get.genes.cor.to(cellexalObj, '",  diffGenes[3], "', output = ", file2Script( paste(tmpFile, 'corrGenes', sep='.' )) ,
	",  is.smarker= FALSE, cpp=TRUE)" ))

wait4server()

expect_true(!file.exists( scriptfile))

expect_true (file.exists( paste(tmpFile, 'corrGenes', sep='.' ) ) )


## more later..


context('server shutdown when pid file is lost')

unlink( pidfile ) ## shut the server down

wait4server()

file.create(scriptfile)
expect_true(file.exists( scriptfile) ,label="after a short wait") ## no strange error?
Sys.sleep( 5 )

expect_true(! file.exists( scriptfile),label= "after five more seconds the server has failed to remove the script file!") ## server is down

Sys.sleep( 10 )

context('slave server shuts down if PID of master becomes inactive')

expect_true( ! isAlive(slavePID), label="slave failed to shut down")



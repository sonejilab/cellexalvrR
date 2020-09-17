context('server VR interface')

if ( .Platform$OS.type != 'unix' ) {
	skip('Test only work on unix platform')
}

if ( is.na( match('cellexalvrR',rownames(installed.packages()))) ) {
	skip("cellexalvrR has to be installed before this test")
}else if ( installed.packages()['cellexalvrR','Version'] != packageDescription("cellexalvrR")$Version) {
	skip ( "Please re-run this test with the updated cellexalvrR lib installed if any error occures" )
}

tmpDir = file.path( tempdir(), 'Output')
dir.create( tmpDir)
tmpDir = file.path( tmpDir, 'TestDataset')
if ( file.exists( tmpDir )){
	unlink( tmpDir, recursive=TRUE)
}
dir.create( tmpDir)

tmpFile = tempfile(pattern = "file", tmpdir = tmpDir, fileext = "")

srvFile =  paste( tmpFile, 'serverR', sep='.')

pidfile    = paste( tmpFile, 'pid', sep='.')
scriptfile = paste( tmpFile, 'input.R', sep=".")
lockfile   = paste( tmpFile, 'input.lock', sep=".") 
pv_file    = paste( tmpFile, 'cellexalvrR.version', sep='.')



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


inspection <- function( ) {
	write_lines( "lockedSave(cellexalObj)" )
	wait4server()
	load( file.path(tmpDir, 'cellexalObj.RData'))
	cellexalObj
}

ls = function(){
	system( paste(sep="",'ls ', tmpDir,"/*"), intern=T)
}

err <- function(){
	system( paste(sep="",'cat ',file.path(tmpDir, "serverError.txt") ), intern=T)
}

## this will become the master we work with
write_lines( c( 
	"library(cellexalvrR)", 
	paste(sep="","setwd('",tmpDir,"')"),
	paste(sep="","server( file='",tmpFile,"', debug=TRUE )" ) ), 
	f= srvFile , 0
)



#startCMD = paste( "paste(", R.exe(),",'CMD BATCH',", file2Script(srvFile),",'&')" )


prefix = '.'
# prefix='tests/testthat'
ipath = file.path( prefix, 'data' )

startCMD = paste( Rscript.exe(), srvFile,"  2>",file.path(tmpDir, "serverError.txt") ,"&" )
#print (startCMD)
system( startCMD )

Sys.sleep(10)
pid = scan( pidfile, quiet=TRUE )

expect_true( isAlive(pid) ,label="server started" )

skip_if ( ! isAlive(pid), "server failed to start" )

write_lines(
	c( 
		paste( sep='', "load('", normalizePath(file.path( ipath, 'cellexalObj.RData')), "')"),
		"print(cellexalObj)" ))

wait4server()

ofiles= ls()

output = ofiles[ grep ('output$', ofiles)]


script <- function(){
	system( paste( 'cat', output ))
}


expect_true( length( output ) == 1, label = "exactly one outfile" )

expect_true( file.exists( output ), label="outfile exists" )

out = read.delim( output, row.names=NULL, header=F )
expect_true( all(dim(out) == c(8,1)), label="incorrect output size")

expect_equal( as.vector(out[2:8,1]), c("An object of class cellexalvrR ",
"with 4709 genes and 1654  cells. ",
"Annotation datasets (0,0): ''   ",
"Sample annotation (1654,23): 'LTHSC_broad', 'LMPP_broad', 'MPP_broad', 'CMP_broad', 'MEP_broad', 'GMP_broad', 'MPP1_broad', 'MPP2_broad', 'MPP3_broad', 'STHSC_broad', 'LTHSC', 'LMPP', 'MPP', 'CMP', 'MEP', 'GMP', 'MPP1', 'MPP2', 'MPP3', 'STHSC', 'ESLAM', 'HSC1', 'Projected'   ",
"There are 0 user group(s) stored :",
"and 3 drc object(s)",
"Specie is set to mouse " ) )

## now we have the correct datast loaded into the server.

write_lines( c(paste(sep="",'cellexalObj@outpath = "',dirname(tmpFile),'"')) )
wait4server()
write_lines( c('print(cellexalObj@outpath)') )
wait4server()
out = read.delim( output, row.names=NULL, header=F )

expect_true( out[nrow(out),] == paste("[1]", dirname(tmpFile)), label=paste("not right path:" , out[nrow(out),]))

############################################################
context('server VR interface - start session')
############################################################


write_lines( c('cellexalObj = sessionPath(cellexalObj, "testSession")'))
wait4server()

expect_true( file.exists( file.path( dirname(tmpFile), 'testSession' )))

for (file in c('1_runRender.R', 'AA_Start_paritalLog.Rmd', '_bookdown.yml', 'png', 'tables') ){
	ofile = file.path( dirname(tmpFile), 'testSession', file )
	expect_true( file.exists( ofile), label=paste("file exists:",ofile))
}

############################################################
context('server VR interface - create heatmap')
############################################################

#  make.cellexalvr.heatmap.list (cvrObj,cellidfile,num.sig,outfile, stats_method=NA)
## first copy the selection file

expect_true( file.copy( file.path(prefix, 'data', 'selection0.txt' ), file.path( tmpDir, 'selection0.txt')), label='selection file copy')

expect_true( file.exists( file.path( tmpDir, 'selection0.txt')))

dir.create(file.path( tmpDir, 'Heatmaps') )

write_lines(paste( sep="",
	"cellexalObj = make.cellexalvr.heatmap.list( cellexalObj, cellidfile = '",
	file.path( tmpDir, 'selection0.txt'),
	"', num.sig = 250, outfile = '",
	file.path(  tmpDir, 'Heatmaps', "testHeatmap" ),
	"', stats_method= 'wilcox')" 
    )
) 

wait4server()

out = read.delim( output, row.names=NULL, header=F )


for ( file in c('testHeatmap','testHeatmap.sqlite3') ){
	expect_true( file.exists( file.path( dirname(tmpFile), 'Heatmaps', file )))
}

expect_true ( length(scan(what=character(), file.path( dirname(tmpFile), 'Heatmaps',"testHeatmap" ) ) ) == 253 , label="always get 253 genes instead of 250?" )

## and now I alsoexpect the results to be in the log!

for ( file in 
	c('2_runRender.R', 'AB_Stats_paritalLog.Rmd', 
		'selection0.txt', 'selection0.txt.group.txt') ){
	expect_true( 
		file.exists( file.path( tmpDir, 'testSession', file ))
		, label=file )
}

## nice - now log the Heatmap -  

############################################################
context('server VR interface - log heatmap')
############################################################


## first create a fake png - why not in the correct place?
png( file=file.path(  tmpDir, 'Heatmaps', "testHeatmap.png"), width=600, height=600 ) 
plot(1:10,1:10)
dev.off()

write_lines(
	paste( sep="",
	"cellexalObj = logHeatmap( cellexalObj, genes= cellexalObj@usedObj$deg.genes, grouping= '",
	file.path( tmpDir, 'selection0.txt'),
	"', png='",file.path( tmpDir, 'Heatmaps', "testHeatmap.png"),"')" 
    )
) 
wait4server()

expect_true( 
		file.exists( file.path( tmpDir, 'testSession', 'AB_Heatmap_paritalLog.Rmd' ))
		, label='AB_Heatmap_paritalLog.Rmd' )

expect_true( 
		file.exists( file.path( tmpDir,"AB_Heatmap_testSession.html")), 
			label="AB_Heatmap_testSession.html" )


############################################################
context('server VR interface - create network')
############################################################


#make.cellexalvr.network (cellexalObj, cellidfile,outpath, 
#    cutoff.ggm=0.1, exprFract = 0.1, top.n.inter=130,method=c("rho.p","pcor"))

write_lines(
	paste( sep="",
		"cellexalObj = make.cellexalvr.network( cellexalObj, '",
		file.path( tmpDir, 'selection0.txt'),
		"', '", file.path( tmpDir, 'Networks' ), "')"
    )
) 

wait4server()

thisP = file.path( tmpDir, 'Networks' )
expect_true( file.exists( thisP), label=thisP )

for (f in c( "Networks.nwk", "NwkCentroids.cnt" )){
	expect_true( file.exists( file.path(thisP, f)),
	 label=file.path(thisP,f) )
}

############################################################
context('server VR interface - log network')
############################################################


#logNetwork ( cellexalObj, genes = NULL, png, grouping, ... )
write_lines(
  paste( sep="",
	"cellexalObj = logNetwork( cellexalObj, grouping= '",
	file.path( tmpDir, 'selection0.txt'),
	"', png='",file.path( tmpDir, 'Heatmaps', "testHeatmap.png"),"')" 
  )
)

wait4server()

thisP = file.path( tmpDir, 'testSession', 'AC_Network_paritalLog.Rmd'  )
expect_true( file.exists(thisP), label = thisP)

thisP =  file.path(tmpDir, 'AC_Network_testSession.html' ) 
expect_true( file.exists(thisP), label = thisP)


############################################################
context('server VR interface - log simple Figure')
############################################################


#logFigure(cellexalObj, png,  text = NULL,...)

write_lines(
  paste( sep="",
	"cellexalObj = logFigure( cellexalObj, ",
	"png='",file.path( tmpDir, 'Heatmaps', "testHeatmap.png"),
	"', text='just a simple figure - ",
	"not really meaningful as that would be created in VR.')" 
  )
)

wait4server()

thisP = file.path(tmpDir, 'AD_Figure_testSession.html')
expect_true( file.exists(thisP), label = thisP)


write_lines(
  paste( sep="",
	"cellexalObj = logFigure( cellexalObj, ",
	"png='",file.path( tmpDir, 'Heatmaps', "testHeatmap.png"),
	"', text='just a simple figure - ",
	"not really meaningful as that would be created in VR.')" 
  )
)

wait4server()

thisP = file.path(tmpDir, 'AE_Figure_testSession.html')
expect_true( file.exists(thisP), label = thisP)

## now shut down the server
############################################################
context('server VR interface - create report - sutdown')
############################################################


write_lines(
  paste( sep="",
	"cellexalObj = renderReport( cellexalObj )" 
  )
)
wait4server()

for ( f in c('testSession.zip','session-log-for-session-testsession.html')){
	thisP = file.path(tmpDir, f)
	expect_true( file.exists(thisP), label = thisP)
}

file.copy( file.path(tmpDir, 'testSession.zip'), file.path( prefix, 'data', 'output', ''  ) )

unlink(  file.path( tempdir(), 'Output'), recursive =TRUE )

Sys.sleep(4)

expect_true( ! isAlive(pid), "server shut down" )


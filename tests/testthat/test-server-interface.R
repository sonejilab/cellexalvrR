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
if ( file.exists(tmpDir )){
	unlink(tmpDir, recursive=TRUE)
}
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
	c( paste( sep="\n",
		paste( sep='', "load('", normalizePath(file.path( ipath, 'cellexalObj.RData')), "')")),
		"cellexalObj = reset(cellexalObj)",
		"print(cellexalObj)" ) )

wait4server()

ofiles= ls()

output = ofiles[ grep ('output$', ofiles)]


script <- function(){
	system( paste( 'cat', output ))
}


expect_true( length( output ) == 1, label = "exactly one outfile" )

expect_true( file.exists( output ), label="outfile exists" )

out = utils::read.delim( output, row.names=NULL, header=F )
expect_true( all(dim(out) == c(8,1)), label="incorrect output size")

expect_equal( as.vector(out[2:8,1]), c("An object of class cellexalvrR ",
"with 4709 genes and 1654  cells. ",
#"Annotation datasets (0,0): ''   ",
"Annotation datasets (4709,2): 'Gene Symbol', 'savekeeping'   ",
"Sample annotation (1654,23): 'LTHSC_broad', 'LMPP_broad', 'MPP_broad', 'CMP_broad', 'MEP_broad', 'GMP_broad', 'MPP1_broad', 'MPP2_broad', 'MPP3_broad', 'STHSC_broad', 'LTHSC', 'LMPP', 'MPP', 'CMP', 'MEP', 'GMP', 'MPP1', 'MPP2', 'MPP3', 'STHSC', 'ESLAM', 'HSC1', 'Projected'   ",
"There are 0 user group(s) stored :",
"and 3 drc object(s)",
"Specie is set to mouse " ) )

## now we have the correct datast loaded into the server.

write_lines( c(paste(sep="",'cellexalObj@outpath = "',dirname(tmpFile),'"')) )
wait4server()
write_lines( c('print(cellexalObj@outpath)') )
wait4server()
out = utils::read.delim( output, row.names=NULL, header=F )

expect_true( out[nrow(out),] == paste("[1]", dirname(tmpFile)), label=paste("not right path:" , out[nrow(out),]))

############################################################
context('server VR interface - start session')
############################################################


write_lines( c('cellexalObj = sessionPath(cellexalObj, "testSession")'))
wait4server()

expect_true( file.exists( file.path( dirname(tmpFile), 'testSession' )))

for (file in c('1_Start_runRender.R', 'AA_Start_paritalLog.Rmd', '_output.yaml', 'png', 'tables') ){
	ofile = file.path( dirname(tmpFile), 'testSession', file )
	expect_true( file.exists( ofile), label=paste("file exists:",ofile))
}

############################################################
context('server VR interface - create heatmap')
############################################################

#  make.cellexalvr.heatmap.list (cvrObj,cellidfile,num.sig,outfile, stats_method=NA)
## first copy the selection file

## to check that the server does not mix up selections

testHeatmap = function( selectionFile, startID ) {

expect_true( file.copy( selectionFile, file.path( tmpDir, basename(selectionFile))), label='selection file copy')

expect_true( file.exists( file.path( tmpDir, basename(selectionFile) )))


if ( ! file.exists( file.path( tmpDir, 'Heatmaps'))){
	dir.create(file.path( tmpDir, 'Heatmaps') )
}

write_lines(paste( sep="",
	"cellexalObj = make.cellexalvr.heatmap.list( cellexalObj, cellidfile = '",
	file.path( tmpDir, basename(selectionFile) ),
	"', num.sig = 250, outfile = '",
	file.path(  tmpDir, 'Heatmaps', paste( sep=".","testHeatmap",startID) ),
	"', stats_method= 'wilcox')" 
    )
) 

wait4server()
Sys.sleep( 1 )

out = utils::read.delim( output, row.names=NULL, header=F )


for ( file in c(
	paste( sep=".","testHeatmap",startID),
	paste( sep=".","testHeatmap",startID,'sqlite3')
	) ){
	expect_true( file.exists( file.path( dirname(tmpFile), 'Heatmaps', file )), 
		label = file )
}

expect_true ( length(scan(what=character(), 
	file.path( dirname(tmpFile), 'Heatmaps',paste(sep=".","testHeatmap" ,startID)) ) ) == 253 , label="always get 253 genes instead of 250?" )

## and now I alsoexpect the results to be in the log!

AA = as.vector( sapply(LETTERS, function(x) paste0(x, LETTERS)))
for ( file in 
	c(paste(sep="", startID,'_Stats_runRender.R'), paste( AA[startID],'_Stats_paritalLog.Rmd',sep=""), 
		basename(selectionFile),paste( sep=".",basename(selectionFile) ,'group.txt') ) ){
	expect_true( 
		file.exists( file.path( tmpDir, 'testSession', file ))
		, label=file )
}

}

testHeatmap( file.path(prefix, 'data', 'selection0.txt' ) , 2 )

## nice - now log the Heatmap -  

############################################################
context('server VR interface - log heatmap')
############################################################

testLogHeatmap = function( startID ) {

## first create a fake png - why not in the correct place?
hfile = paste( sep=".","testHeatmap",startID,'png')
png( file=file.path(  tmpDir, 'Heatmaps',hfile), width=600, height=600 ) 
plot(1:10,1:10, main= hfile )
grDevices::dev.off()

write_lines(
	paste( sep="",
	"cellexalObj = logHeatmap( cellexalObj, genes= cellexalObj@usedObj$deg.genes, grouping= '",
	file.path( tmpDir, 'selection10.txt'),
	"', png='",file.path( tmpDir, 'Heatmaps', hfile ),"')" 
    )
) 
wait4server()
Sys.sleep( 1 )

AA = as.vector( sapply(LETTERS, function(x) paste0(x, LETTERS)))
ofile = file.path( tmpDir, 'testSession', paste(sep='_', AA[startID],'Heatmap_paritalLog.Rmd' ))
expect_true( file.exists( ofile ), label=ofile )


ofile = file.path( tmpDir,paste(sep='_', AA[startID],"Heatmap_testSession.html" ) )
expect_true( file.exists( ofile ), label=ofile )

}
testLogHeatmap( 3 )

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
Sys.sleep( 1 )

thisP = file.path( tmpDir, 'Networks' )
expect_true( file.exists( thisP), label=thisP )

for (f in c( "Networks.nwk", "NwkCentroids.cnt" )){
	expect_true( file.exists( file.path(thisP, f)),
	 label=file.path(thisP,f) )
}

############################################################
context('server VR interface - log network')
############################################################


#logNetworkPNG ( cellexalObj, genes = NULL, png, grouping, ... )
hfile = paste( sep=".","testNetwork",'png')
grDevices::png( file=file.path(  tmpDir, 'Heatmaps',hfile), width=600, height=600 ) 
plot(1:10,1:10, main= "Network" )
grDevices::dev.off()

write_lines(
  paste( sep="",
	"cellexalObj = logNetworkPNG( cellexalObj, grouping= '",
	file.path( tmpDir, 'selection0.txt'),
	"', png='",file.path( tmpDir, 'Heatmaps', "testNetwork.png"),"')" 
  )
)

wait4server()
Sys.sleep( 1 )


thisP = file.path( tmpDir, 'testSession', 'AD_Network_paritalLog.Rmd'  )
expect_true( file.exists(thisP), label = thisP)

thisP =  file.path(tmpDir, 'AD_Network_testSession.html' ) 
expect_true( file.exists(thisP), label = thisP)


############################################################
context('server VR interface - log simple Figure')
############################################################


#logFigure(cellexalObj, png,  text = NULL,...)


write_lines(
  paste( sep="",
	"cellexalObj = logFigure( cellexalObj, ",
	"png='",file.path( tmpDir, 'Heatmaps', "testNetwork.png"),
	"', text='just a simple figure - ",
	"not really meaningful as that would be created in VR.')
	" 
  )
)

wait4server()
Sys.sleep( 1 )

thisP = file.path(tmpDir, 'AE_Figure_testSession.html')
expect_true( file.exists(thisP), label = thisP)


write_lines(
  paste( sep="",
	"cellexalObj = logFigure( cellexalObj, ",
	"png='",file.path( tmpDir, 'Heatmaps', "testNetwork.png"),
	"', text='just a simple figure - ",
	"not really meaningful as that would be created in VR.')
	" 
  )
)

#browser()
wait4server()
Sys.sleep( 1 )

thisP = file.path(tmpDir, 'AF_Figure_testSession.html')
expect_true( file.exists(thisP), label = thisP)



############################################################
context('server VR interface - create timeline')
############################################################

#  make.cellexalvr.heatmap.list (cvrObj,cellidfile,num.sig,outfile, stats_method=NA)
## first copy the selection file

gFile= 'SelectionHSPC_time.txt'
grouping <- file.path(prefix, 'data', gFile )

expect_true( file.copy( grouping, file.path( tmpDir, gFile)), label='selection file copy')

expect_true( file.exists( file.path( tmpDir, gFile)))

#dir.create(file.path( tmpDir, 'Heatmaps') )

write_lines(paste( sep="",
	"cellexalObj = make.cellexalvr.heatmap.list( cellexalObj, cellidfile = '",
	file.path( tmpDir, gFile),
	"', num.sig = 250, outfile = '",
	file.path(  tmpDir, 'Heatmaps', "timeHeatmap" ),
	"', stats_method= 'wilcox')" 
    )
) 

wait4server()
Sys.sleep( 1 )

out = utils::read.delim( output, row.names=NULL, header=F )


for ( file in c('timeHeatmap','timeHeatmap.sqlite3') ){
	expect_true( file.exists( file.path( dirname(tmpFile), 'Heatmaps', file )), label = file )
}


l = length(scan(what=character(), file.path( dirname(tmpFile), 'Heatmaps',"timeHeatmap" ) ) )
expect_true ( l == 251 , 
     label=paste("always get 251 genes instead of 250? (",l,")" ) )

## and now I alsoexpect the results to be in the log!
Sys.sleep( 1 )

for ( file in 
	c('8_OneGroupTime_runRender.R', 'AH_OneGroupTime_paritalLog.Rmd', 
		'SelectionHSPC_time.txt', 'SelectionHSPC_time.txt.group.txt',
		'SelectionHSPC_time.txt.time','SelectionHSPC_time.txt.time.points') ){
	expect_true( 
		file.exists( file.path( tmpDir, 'testSession', file ))
		, label=file )
}

ofile= file.path( tmpDir,"AG_Stats_testSession.html")
expect_true( file.exists( ofile), label=ofile ) 

ofile= file.path( tmpDir,"AH_OneGroupTime_testSession.html")
expect_true( file.exists( ofile), label=ofile ) 



### check whether the heatmap is able to find "it's" selection if the logHeatmap gets faulty info.
############################################################
context('server VR interface - second heatmap')
############################################################

testHeatmap( file.path(prefix, 'data', 'selection1.txt' ) , 9 )

testLogHeatmap( 10 )


## now shut down the server
############################################################
context('server VR interface - create report - shutdown')
############################################################


write_lines(
  paste( sep="",
	"cellexalObj = renderReport( cellexalObj )" 
  )
)
wait4server()

if ( file.exists(file.path(prefix, 'data', 'output', 'TestDataset') )){
	unlink( file.path(prefix, 'data', 'output', 'TestDataset'), recursive=TRUE )
}
dir.create( file.path(prefix, 'data', 'output', 'TestDataset') )
file.copy( tmpDir,  file.path(prefix, 'data', 'output', 'TestDataset'), recursive=TRUE)

#for ( f in c('testSession.zip','session-log-for-session-testsession.html')){
#	thisP = file.path(tmpDir, f)
#	expect_true( file.exists(thisP), label = thisP)
#}

#file.copy( file.path(tmpDir, 'testSession.zip'), file.path( prefix, 'data', 'output', ''  ) )

unlink(  file.path( tempdir(), 'Output'), recursive =TRUE )

Sys.sleep(4)

expect_true( ! isAlive(pid), "server shut down" )


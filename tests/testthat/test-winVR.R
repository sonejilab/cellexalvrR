context('server VR interface')

if ( .Platform$OS.type != 'windows' ) {
	skip('More in detail test for linux available')
}

if ( is.na( match('cellexalvrR',rownames(installed.packages()))) ) {
	skip("cellexalvrR has to be installed before this test")
}else if ( installed.packages()['cellexalvrR','Version'] != packageDescription("cellexalvrR")$Version) {
	skip ( "Please re-run this test with the updated cellexalvrR lib installed if any error occures" )
}

prefix = '.'
# prefix='tests/testthat'

tmpDir = file.path( prefix,'data', 'output', 'winVR' )

if ( file.exists(tmpDir )){
	unlink(tmpDir, recursive=TRUE)
}
dir.create( tmpDir)

cellexalObj = reset ( cellexalObj )

cellexalObj@outpath = tmpDir

cellexalObj = sessionPath( cellexalObj, 'testSession')

expect_true( file.exists( file.path( tmpDir, 'testSession' )), label="session path created" )

for (file in c('1_Start_runRender.R', 'AA_Start_paritalLog.Rmd', '_output.yaml', 'png', 'tables') ){
	ofile = file.path(tmpDir, 'testSession', file )
	expect_true( file.exists( ofile), label=paste("file exists:",ofile))
}

############################################################
context('server VR interface - create heatmap')
############################################################

#  make.cellexalvr.heatmap.list (cvrObj,cellidfile,num.sig,outfile, stats_method=NA)
## first copy the selection file

## to check that the server does not mix up selections
selectionFile = file.path(prefix, 'data', 'selection0.txt' )

cellexalObj = make.cellexalvr.heatmap.list( 
	cellexalObj, 
	cellidfile = selectionFile,
	num.sig = 250, 
	outfile = file.path(  tmpDir, 'Heatmaps', "testHeatmap_ABXYFSD" ),
	stats_method= 'wilcox'
)

for ( file in c('testHeatmap_ABXYFSD','testHeatmap_ABXYFSD.sqlite3') ){
		expect_true( file.exists( file.path( tmpDir, 'Heatmaps', file )), 
			label = file )
}

expect_true ( length(scan(what=character(), 
	file.path( tmpDir, 'Heatmaps','testHeatmap_ABXYFSD' ) )) == 253 , 
label="always get 253 genes instead of 250?" )

## and now I also expect the results to be in the log!
startID = 2
AA = as.vector( sapply(LETTERS, function(x) paste0(x, LETTERS)))

for ( file in 
		c(paste(sep="", startID,'_Stats_runRender.R'), paste( AA[startID],'_Stats_paritalLog.Rmd',sep=""), 
			basename(selectionFile),paste( sep=".",basename(selectionFile) ,'group.txt') ) ){
		expect_true( 
			file.exists( file.path( tmpDir, 'testSession', file ))
			, label=file )
}


############################################################
context('server VR interface - create network')
############################################################


#make.cellexalvr.network (cellexalObj, cellidfile,outpath, 
#    cutoff.ggm=0.1, exprFract = 0.1, top.n.inter=130,method=c("rho.p","pcor"))

cellexalObj = make.cellexalvr.network( cellexalObj, 
	selectionFile,
  file.path( tmpDir, "Resources", 'Networks' )
)


thisP = file.path( tmpDir, "Resources", 'Networks' )
expect_true( file.exists( thisP), label=thisP )

for (f in c( "Networks.nwk", "NwkCentroids.cnt" )){
	expect_true( file.exists( file.path(thisP, f)),
	 label=file.path(thisP,f) )
}

thisP = file.path( tmpDir, 'testSession', 'AC_Networks_paritalLog.Rmd'  )
expect_true( file.exists(thisP), label = thisP)

thisP =  file.path(tmpDir, 'AC_Networks_testSession.html' ) 
expect_true( file.exists(thisP), label = thisP)

############################################################
context('server VR interface - log network')
############################################################


#logNetwork ( cellexalObj, genes = NULL, png, grouping, ... )
hfile = paste( sep=".","testNetwork",'png')
grDevices::png( file=file.path(  tmpDir, 'Heatmaps',hfile), width=600, height=600 ) 
plot(1:10,1:10, main= "Network" )
grDevices::dev.off()

cellexalObj = logNetwork( cellexalObj, grouping= 
	file.path( tmpDir, 'selection0.txt'), 
	png= file.path( tmpDir, 'Heatmaps', "testNetwork.png")
)


thisP = file.path( tmpDir, 'testSession', 'AD_Network_paritalLog.Rmd'  )
expect_true( file.exists(thisP), label = thisP)

thisP =  file.path(tmpDir, 'AD_Network_testSession.html' ) 
expect_true( file.exists(thisP), label = thisP)

## now shut down the server
############################################################
context('server VR interface - create report - shutdown')
############################################################


cellexalObj = renderReport( cellexalObj )


files= c(  "_bookdown.yml" , "_output.yaml",
 "1_Start_runRender.R" ,"2_Stats_runRender.R",
 "3_Networks_runRender.R" ,"4_Network_runRender.R",
 "AA_1_Networks.nwk" , "AA_2_Networks.nwk",
 "AA_3_Networks.nwk" ,  "AA_Start_paritalLog.Rmd",
 "AANetworks.nwk"  ,   "AANwkCentroids.cnt",
 "AB_Stats_paritalLog.Rmd"  , "AC_Networks_paritalLog.Rmd",
 "AD_Network_paritalLog.Rmd" , "AE_End_paritalLog.Rmd",
 "png"    ,  "PseudoBulk.User.group.1..RData",
 "PseudoBulk.User.group.1.gct" , "runRender.R",
 "selection0.txt"  ,  "selection0.txt.group.txt",
 "table.css"  , "tables"
)

li = list()
for (x in list.files(file.path(tmpDir, 'testSession')) ){ li[[x]] =1}

for( f in files ){
	expect_true( li[[f]] == 1, label=paste("expected file", f))
}



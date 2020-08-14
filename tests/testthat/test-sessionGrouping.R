context('create session grouping')

prefix = './'

data = file.path(prefix, 'data/cellexalObj.RData')

cellexalObj = loadObject( data )

cellexalObj@userGroups=data.frame()
cellexalObj@usedObj$lastGroup = NULL
cellexalObj@usedObj$SelectionFiles = list()

datadir <- normalizePath(file.path( prefix, 'data', 'output'))
cellexalObj@usedObj$sessionPath = cellexalObj@usedObj$sessionRmdFiles = cellexalObj@usedObj$sessionName = NULL

cellexalObj@outpath = file.path(datadir) ## to not mess up the package

## init a session
lockedSave(cellexalObj)
if ( file.exists( file.path(datadir, 'sessionGroupingTest' )) ){
	#t = lapply( list.files(file.path(datadir) , 
	#	full.names = TRUE, recursive = TRUE), unlink )
	unlink(  file.path(datadir, 'sessionGroupingTest' ), recursive=TRUE )
}

dir.create( file.path(datadir, 'sessionGroupingTest' ) )
fnames = ( c( 
file.path(datadir,"1_Start_sessionGroupingTest.html") , 
file.path(datadir,"2_Heatmap_sessionGroupingTest.html"),
file.path(datadir,"3_Network_sessionGroupingTest.html"),
file.path(datadir,"3_Stats_sessionGroupingTest.html"),
file.path(datadir,"4_Ontology_sessionGroupingTest.html"),
file.path(datadir,'sessionGroupingTest',"1_Start_paritalLog.Rmd"),
file.path(datadir,'sessionGroupingTest',"2_Heatmap_paritalLog.Rmd"),
file.path(datadir,'sessionGroupingTest',"3_Network_paritalLog.Rmd"),
file.path(datadir,'sessionGroupingTest',"3_Stats_paritalLog.Rmd"),
file.path(datadir,'sessionGroupingTest',"4_Ontology_paritalLog.Rmd")

))
t = lapply ( fnames, file.create)

sessionPath( cellexalObj, 'sessionGroupingTest' )
for ( n in fnames[-c(1,6)] ) { expect_true( ! file.exists(n), paste("file not removed",n)) }
for ( n in fnames[c(1,6)] ) { expect_true( file.exists(n), paste("file not created",n)) }

lapply( list.files(file.path(datadir) , 
		full.names = TRUE, recursive = TRUE), unlink )

cellexalObj = sessionPath( cellexalObj, 'sessionGroupingTest' )

expect_true( file.exists( file.path(datadir, 'sessionGroupingTest' ) ) , "session path has not been created" )

lapply( list.files(datadir , full.names = TRUE, recursive = FALSE), unlink )

grouping =  file.path(prefix, 'data', 'selection0.txt' )


cellexalObj = userGrouping( cellexalObj, grouping )
cellexalObj = sessionRegisterGrouping( cellexalObj, cellexalObj@usedObj$lastGroup )
n = sessionCounter( cellexalObj, cellexalObj@usedObj$lastGroup )

expect_true( n == 1, paste("first entry not 1(", n, ")"))


grouping =  file.path(prefix, 'data', 'selection1.txt' )

cellexalObj = userGrouping( cellexalObj, grouping )
cellexalObj = sessionRegisterGrouping( cellexalObj, cellexalObj@usedObj$lastGroup )
n = sessionCounter( cellexalObj, cellexalObj@usedObj$lastGroup )

expect_true( n == 2, paste("second entry not 2(", n, ")"))

grouping =  file.path(prefix, 'data', 'selection0.txt' )
cellexalObj = userGrouping( cellexalObj, grouping )
n = sessionCounter( cellexalObj, cellexalObj@usedObj$lastGroup )

expect_true( n == 1, paste("third try: first entry not 1(", n, ")"))

## now add some Session reports:

genes <- file.path(prefix, 'data/heatmap_0.txt')
if ( ! file.exists(file.path(datadir,  'tmp')) ){
	dir.create(file.path(datadir, 'tmp'), recursive = TRUE )
}

png( file=file.path(datadir, 'tmp', 'a_simple_figure.png'), width=800, height=800 )
plot(1:100, sample(100:1, 100), main="Just for the test 1!" )
dev.off()


heatmap_png <- file.path(datadir,  'tmp', 'a_simple_figure.png')

grouping <- file.path(prefix, 'data/selection0.txt')

ontology <- 'BP'

topNodes  <- 20

ofiles = c('2_Heatmap_paritalLog.Rmd', 'png/User.group.1.DDRtree.1_2.png', 'png/a_simple_figure.png','png/User.group.1.DDRtree.2_3.png' )

for ( fname in ofiles ){
	
	if( file.exists( file.path( datadir,  'sessionGroupingTest', fname ) ) ) {
		file.remove( file.path( datadir,  'sessionGroupingTest', fname ) )
	}
}
ofile=  file.path( datadir, '2_Heatmap_sessionGroupingTest.html')

if( file.exists(ofile)) {
	unlink(ofile)
}


#system( paste( 'Rscript', script, datadir, genes, heatmap_png, grouping, ontology, topNodes ))
cellexalObj = logHeatmap(cellexalObj, genes, heatmap_png, grouping, ontology = ontology, topNodes = topNodes )


for ( fname in c( ofiles ) ){
	expect_true( file.exists( file.path(datadir, 'sessionGroupingTest',  fname ) ) , paste( "file has not been created",file.path(datadir, 'sessionGroupingTest', fname) ))
}
expect_true( file.exists( ofile), paste( "file has not been created", ofile))

#now lets try the logStatResult funtion:

test = data.frame( A = rep(0,10), B= rep(1,10), 'p_val'= c( 1e-9, 1e-8, 1e-7, 1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1) )

if ( file.exists( file.path(datadir, '3_Stats_sessionGroupingTest.html' ))) {
	unlink(  file.path(datadir, '3_Stats_sessionGroupingTest.html' ) )
}
logStatResult(cellexalObj, 'SimpleTest', test, 'p_val' )

expect_true( file.exists( file.path(datadir, '3_Stats_sessionGroupingTest.html' )), 'logStatResult failed')


# logNetwork

if ( file.exists( file.path(datadir, '3_Network_sessionGroupingTest.html' ))) {
	unlink(  file.path(datadir, '3_Network_sessionGroupingTest.html' ) )
}
cellexalObj = logNetwork(cellexalObj,  png =  heatmap_png , grouping= grouping )
expect_true( file.exists( file.path(datadir, '3_Network_sessionGroupingTest.html' )), 'logNetworks failed')

## ontologyLogPage

if ( file.exists( file.path(datadir, '4_Ontology_sessionGroupingTest.html' ))) {
	unlink(  file.path(datadir, '4_Ontology_sessionGroupingTest.html' ) )
}
cellexalObj = ontologyLogPage(cellexalObj,  genes=genes , grouping= grouping )
expect_true( file.exists( file.path(datadir, '4_Ontology_sessionGroupingTest.html' )), 'ontologyLog failed')


ofile=  file.path( datadir, 'session-log-for-session-sessiongroupingtest.html')
if( file.exists(ofile)) {
	unlink(ofile)
}

cellexalObj = renderReport ( cellexalObj )

expect_true(file.exists( ofile), 'html report / padoc installed?')

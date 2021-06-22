context('filterCapture')


prefix = './'
#prefix = 'tests/testthat'

cellexalObj = reset(cellexalObj)
cellexalObj@outpath =  file.path( prefix, 'data', 'output','Filters')
if ( file.exists( cellexalObj@outpath )){
	unlink(cellexalObj@outpath, recursive=TRUE)
}
dir.create(cellexalObj@outpath)

cellexalObj = sessionPath( cellexalObj, 'noServer' )

oldFilters = newFilters( c(), path = cellexalObj@outpath )

expect_equal( newFilters( oldFilters, path = cellexalObj@outpath ), c(), label="no new filters" )

cat("(gene:ifitm1 > 20 && (gene:cd34 > 0 && gene:cd34 < 15))",file=file.path(cellexalObj@outpath, "ifitm1_cd34.fil"),sep="\n")


newFilters = newFilters( oldFilters, path = cellexalObj@outpath )

expect_equal(length( newFilters ), 1, label="one new filter" )

path = cellexalObj@usedObj$sessionPath

## grab this from the server.R function:
newFilters = newFilters( oldFilters, path=cellexalObj@outpath )
if ( length(newFilters) > 0 ){
	for (n in newFilters) {
		this = file.path( cellexalObj@usedObj$sessionPath, n )
		n = file.path(cellexalObj@outpath, n)
		message( paste( 'logFigure will log the file',n ))
		file.copy( n, this)
		filter= paste( collapse=" ", scan( this, what=character()))
		content = paste("\n## New Filter\n\nA new filter has been created in VR:\n\n```", filter,"```\n\n")
		cellexalObj = storeLogContents( cellexalObj, content, type="VRfilter" )
		id = length(cellexalObj@usedObj$sessionRmdFiles)
		cellexalObj = renderFile( cellexalObj, id, type='VRfilter' )
	}
	oldFilters = c( oldFilters, newFilters)
}

cellexalObj = renderReport( cellexalObj )

source('function.R')

collect = list( 
	'New Filter' = 0,
	'Filters' = 0,
	'noServer' = 0,
	'Session End' = 0,
	"gene:ifitm1" = 0
)

files = c( "cellexalObj.RData", "ifitm1_cd34.fil", "libs", "noServer", 
	"PortableLog_noServer.zip", "reference-keys.txt", "search_index.json",
	 "session-log-for-session-noserver.html" )
for ( f in files) {
	expect_true(file.exists( file.path(cellexalObj@outpath, f) ), label = f)
}
collect = checkFile( collect, file.path(cellexalObj@outpath,"session-log-for-session-noserver.html"))

expect = list( 
	'New Filter' = 2,
	'Filters' = 1,
	'noServer' = 8,
	'Session End' = 2,
	"gene:ifitm1" = 1
)

expect_equal( collect, expect, label="session log contents")

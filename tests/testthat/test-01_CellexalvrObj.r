context('error throwing') 
expect_error( loadObject( 'SomeFileNotExistsing') )

context('export2cellexalvr function')
prefix = '.'
opath = file.path( prefix, 'data','output' )
ipath = file.path( prefix, 'data' )
if ( file.exists(opath)) {
	unlink( opath, recursive=TRUE)
}
dir.create(opath,  showWarnings = FALSE, recursive = TRUE)

context('minimal cellexalvrR object') 
m = matrix( floor(runif(30000, min=-1000, max=101)), ncol=300)
colnames(m) = paste('cell', 1:ncol(m))
rownames(m) = paste('gene', 1:nrow(m))
m[which(m< 1)] = 0
m = Matrix::Matrix(m,sparse=T)
obj = new( 'cellexalvrR', data=m , drc= list('test' = cbind(x=runif(300), y=runif(300), z=runif(300) )) )

obj = check( obj )


expect_true( obj@usedObj$checkPassed == FALSE,
	"The internal check shold fail" )


## fix the object first
rownames(obj@drc[[1]]) = colnames(obj@data)
obj@meta.cell = make.cell.meta.from.df ( data.frame( 'a' = sample( c('A','B'), replace=T, 300), 'B' = sample( c('C','D','E'), replace=T, 300) ), c('a','B') )
rownames(obj@meta.cell) =  colnames(obj@data)

obj = check( obj )


expect_true( obj@usedObj$checkPassed == TRUE,
	"The internal check suceeds" ) 


export2cellexalvr( obj, opath )


if ( file.exists(file.path(ipath,'cellexalObjOK.RData.lock')) ) {
	unlink(file.path(ipath, 'cellexalObjOK.RData.lock') )
}
if ( ! file.exists (file.path(ipath,'cellexalObjOK.RData') ) ) {
	stop( paste("Libraray error - test file not found ", file.path(ipath,'cellexalObjOK.RData')) )
}
cellexalObj <- loadObject(file.path(ipath,'cellexalObjOK.RData'))

ofiles = c( 'a.meta.cell', 'c.meta.gene', 'database.sqlite', 'DDRtree.mds', 
		 'index.facs',  'diffusion.mds', 'tSNE.mds' )


for ( f in ofiles ) {
	ofile = file.path(opath, f ) 
	if(  file.exists(ofile ) ){
		unlink( ofile)
	}
}
if (! file.exists( opath)) {
	dir.create(opath )
	dir.create( file.path(opath, 'default_user') )
	dir.create( file.path(opath, 'default_user','testSession'))
	dir.create( file.path(opath, 'default_user','testSession', 'tables'),  recursive = TRUE)
}

export2cellexalvr(cellexalObj , opath )


for ( f in ofiles ) {
	ofile = file.path(opath, f ) 
	expect_true( file.exists( ofile ), paste("outfile exists", ofile) )
}



context( "store user groupings" )

if ( file.exists( file.path(cellexalObj@outpath, 'initialTest')) ){
	 unlink(file.path(cellexalObj@outpath, 'initialTest'), recursive=TRUE)
}

cellexalObj@outpath = opath
cellexalObj = sessionPath(cellexalObj, 'initialTest' )

old_length = 0
cellexalObj@userGroups = data.frame()

cellexalObj = userGrouping(cellexalObj, file.path(ipath, 'selection0.txt') )
expect_equal( length(cellexalObj@userGroups) , old_length + 2 )

## but also test whether the file was read correctly!! Epic bug went undetected!!
ids = cellexalObj@userGroups[,1] 
names(ids) = colnames(cellexalObj@data)
orig = read.delim( file.path(ipath, 'selection0.txt'), header=F)
origids = orig[,4] +1
names(origids) = orig[,1]
m = match(names(origids), names(ids) ) ## likely some missing
expect_true( all.equal(origids, ids[m]) == TRUE, 'grouping stored correctly')
## and check the order and the colors, too

expect_true( all.equal(cellexalObj@userGroups[m,2], 1:length(m)) == TRUE, 
	'order stored correctly' )
expect_true( all.equal(cellexalObj@colors[[1]], as.vector(unique(orig[,2]))) == TRUE, 
	'color stored correctly' )

expect_true( file.exists( file.path(cellexalObj@outpath, 'initialTest', 'selection0.txt')), 
	"the selction has not been copied to the session path")
expect_true( file.exists( file.path(cellexalObj@outpath, 'initialTest', 'selection0.txt.group.txt')), 
	"the selction's internal colname is not stored")

cellexalObj = userGrouping(cellexalObj, file.path(ipath, 'selection0.txt') )
expect_equal( length(cellexalObj@userGroups) ,old_length +  2 ) # same grouing no adding of the data

context( "heatmap is produced" )
ofile = file.path(opath, 'heatmaps','testHeatmap.txt') 

if ( ! file.exists( file.path(opath, 'heatmaps')) ){
	dir.create( file.path(opath, 'heatmaps') )
}
if(  file.exists(ofile ) ){
	unlink( ofile)
}
if(  file.exists(paste( ofile , '.sqlite3', sep="")) ){
	unlink( paste( ofile , '.sqlite3', sep="") )
}

load(system.file( 'data/cellexalObj.rda', package='cellexalvrR'))
cellexalObj@outpath = opath
lockedSave( cellexalObj )

make.cellexalvr.heatmap.list ( file.path(opath, 'cellexalObj.RData') , file.path(ipath,'selection0.txt'), 300, ofile )

expect_true( file.exists( ofile ),  paste("gene list file missing:", ofile) )


expect_true( file.exists( paste( ofile , '.sqlite3', sep="") ),  paste("heatmap database file missing:", ofile) )

context('toGSEA')

#prefix = 'tests/testthat'
prefix = '.'

ofile = file.path( prefix, 'data', 'output', 'testGSEAfile' )

cellexalObj=reset(cellexalObj)
cellexalObj@outpath = ofile

if ( file.exists( ofile) ) {
	unlink(ofile, recursive=TRUE)
}
dir.create(ofile, recursive=TRUE)

cellexalObj = sessionPath(cellexalObj, 'toGSEA')


selection = file.path( prefix, 'data', 'selection10.txt' )

expect_true (file.exists( selection) , label="Important selection file missing" )

cellexalObj = userGrouping( cellexalObj, selection )

info = groupingInfo(cellexalObj)
cellexalObj = reduceTo( cellexalObj, what='col', to=names(info@grouping)[which(!is.na(info@grouping))] )
cellexalObj = pseudoBulk( groupingInfo(cellexalObj), cellexalObj )

toGSEA( cellexalObj, file.path( cellexalObj@usedObj$sessionPath, 'test..RData') , cellexalObj@usedObj$lastGroup )


expect_true( file.exists(file.path( cellexalObj@usedObj$sessionPath,  paste( 'test' , 'gct', sep=".") ) ), label='gct file exists')

toGSEA( cellexalObj, file.path( cellexalObj@usedObj$sessionPath, 'test..RData') , cellexalObj@usedObj$lastGroup )

expect_true( file.exists( file.path( cellexalObj@usedObj$sessionPath, paste( 'test' , '1', 'gct', sep=".") ) ), label='gct file exists')

ofile = file.path( cellexalObj@usedObj$sessionPath,  paste( 'test' , 'gct', sep=".") )

df =  read.delim( ofile, skip =2 )

expect_equal( dim(df), c(4709, 86 ), label="pseudo bulk data")

expect_true ( all.equal( df[,1],  rownames(cellexalObj@data) ) ==TRUE, label="Gene Names")

expect_true ( all.equal( df[,2],  rep( NA, nrow(df) ) ) ==TRUE, label="Descriptions")


cellexalObj = renderReport(cellexalObj)
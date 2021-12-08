context('toGSEA')

#prefix = 'tests/testthat'
prefix = '.'

ofile = file.path( prefix, 'data', 'output', 'testGSEAfile' )

fileConn<-file( paste( ofile, 'GCT', sep=".") )
writeLines(c("This must not be in the final file!" ), fileConn)
close(fileConn)

selection = file.path( prefix, 'data', 'selection10.txt' )

expect_true (file.exists( selection) , label="Important selection file missing" )

cellexalObj = userGrouping( cellexalObj, selection )

toGSEA( cellexalObj, ofile , cellexalObj@usedObj$lastGroup )


df =  read.delim( paste( ofile, 'GCT', sep="."), skip =2 )

expect_true ( all.equal( df[,1],  rownames(cellexalObj@data) ) ==TRUE, label="Gene Names")

expect_true ( all.equal( df[,2],  rep( NA, nrow(df) ) ) ==TRUE, label="Descriptions")


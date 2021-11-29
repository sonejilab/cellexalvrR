context('stat functions singleCells')

prefix = './'

if ( file.exists( file.path(prefix, 'function.R'))){
	## the test functions that should not be maintained in differet test scripts...
	source( file.path(prefix, 'function.R')) 

}

if ( file.exists( file.path(prefix,'data','output','statTest','statTestSingleC' ) )){
	unlink(  file.path(prefix,'data','output','statTest','statTestSingleC' ), recursive=TRUE )
}

x = reset(cellexalObj)
x@outpath = file.path(prefix,'data','output','statTest' )
x = sessionPath( x, 'statTestSingleC' )


if ( ! file.exists(x@outpath)){
	dir.create(x@outpath)
}

grouping <- file.path(prefix, 'data/selection0.txt')

gene2 = getDifferentials(x, grouping, 'wilcox', num.sig=100, Log=FALSE, logfc.threshold = .1, minPct=0.1, bulkStats=FALSE )

expect_true( length( gene2@usedObj$deg.genes) == 102, info = paste("wrong gene number c++ wilcox", length( gene2) ) )
#logStatResult ( gene2, gene2@usedObj$sigGeneLists$Cpp[[gene2@usedObj$lastGroup]],
#method= 'wilcox', 'p.value')
ofile=  file.path( x@outpath, 'AB_Stats_statTestSingleC.html' )
expect_true( file.exists( ofile), label = ofile)


ofile=  file.path( x@outpath, 'session-log-for-session-stattestsinglec.html' )
if ( file.exists( ofile))
	unlink(ofile)

x = renderReport(x)

expect_true( file.exists( ofile), label="stats main outfile")

search = list(
	'User.group.1 Statistical Result' = 0,
	'./statTestSingleC/tables/User.group.1.Cpp.csv' =0,
	"statTestSingleC/png/User.group.1.DDRtree.1_2.png"= 0,
	"statTestSingleC/png/User.group.1.DDRtree.1_3.png"= 0,
	"statTestSingleC/png/User.group.1.DDRtree.2_3.png"= 0,
	'pseudo bulk samples .n.' = 0
)

search = checkFile( search, ofile )

exp= list(
	'User.group.1 Statistical Result' = 2,
	'./statTestSingleC/tables/User.group.1.Cpp.csv' =1,
	"statTestSingleC/png/User.group.1.DDRtree.1_2.png"= 1,
	"statTestSingleC/png/User.group.1.DDRtree.1_3.png"= 1,
	"statTestSingleC/png/User.group.1.DDRtree.2_3.png"= 1,
	'pseudo bulk samples .n.' = 0
)

expect_equal( search, exp, label="html main file internals")

for ( fn in names(search)[3:5]) {
	expect_true(file.exists( file.path(x@outpath, fn) ), label=fn)
}


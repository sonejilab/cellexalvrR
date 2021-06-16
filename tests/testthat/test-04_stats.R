context('stat functions')

prefix = './'

if ( file.exists( file.path(prefix, 'function.R'))){
	## the test functions that should not be maintained in differet test scripts...
	source( file.path(prefix, 'function.R')) 

}

x = reset(cellexalObj)
x@outpath = file.path(prefix,'data','output','statTest' )
x = sessionPath( x, 'StatTest' )


if ( ! file.exists(x@outpath)){
	dir.create(x@outpath)
}

grouping <- file.path(prefix, 'data/selection0.txt')

gene2 = getDifferentials(x, grouping, 'wilcox', num.sig=100, Log=FALSE, logfc.threshold = .1, minPct=0.1 )

expect_true( length( gene2@usedObj$deg.genes) == 102, info = paste("wrong gene number c++ wilcox", length( gene2) ) )
#logStatResult ( gene2, gene2@usedObj$sigGeneLists$Cpp[[gene2@usedObj$lastGroup]],
#method= 'wilcox', 'p.value')
ofile=  file.path( x@outpath, 'AB_Stats_StatTest.html' )
expect_true( file.exists( ofile), label = ofile)


ofile=  file.path( x@outpath, 'session-log-for-session-stattest.html' )
if ( file.exists( ofile))
	unlink(ofile)

x = renderReport(x)

expect_true( file.exists( ofile), label="stats main outfile")

search = list(
	'statistical-result-from-user.group.1' = 0,
	'./StatTest/tables/User.group.1.Cpp.csv' =0,
	"StatTest/png/User.group.1.DDRtree.1_2.png"= 0,
	"StatTest/png/User.group.1.DDRtree.1_3.png"= 0,
	"StatTest/png/User.group.1.DDRtree.2_3.png"= 0
)

search = checkFile( search, ofile )

exp= list(
	'statistical-result-from-user.group.1' = 2,
	'./StatTest/tables/User.group.1.Cpp.csv' =1,
	"StatTest/png/User.group.1.DDRtree.1_2.png"= 1,
	"StatTest/png/User.group.1.DDRtree.1_3.png"= 1,
	"StatTest/png/User.group.1.DDRtree.2_3.png"= 1
)

expect_equal( search, exp, label="html main file internals")

for ( fn in names(search)[3:5]) {
	expect_true(file.exists( file.path(x@outpath, fn) ), label=fn)
}




#gene1 = getDifferentials(x, grouping, 'anova', num.sig=100 )




## get a grouping added?

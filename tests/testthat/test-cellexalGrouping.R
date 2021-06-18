context('cellexalGrouping class')

prefix = '.'

if ( file.exists( file.path(prefix, 'function.R'))){
	## the test functions that should not be maintained in differet test scripts...
	source( file.path(prefix, 'function.R')) 

}
#genes <- file.path(prefix, 'data/heatmap_0.txt')

#genes = utils::read.delim(genes)[,1]

#cellexalObj <- loadObject(file.path(prefix,'data','cellexalObjOK.RData') )

x = reset(cellexalObj)
x@outpath = file.path(prefix,'data','output','cellexalGrouping' )

if ( ! file.exists(x@outpath)){
	dir.create(x@outpath)
}

x = sessionPath( x, 'cellexalGroupTest' )

grouping <- file.path(prefix, 'data/selection1.txt')
x = userGrouping(x, grouping )

fn = drcPlots2D( x, 'User.group.1')

expect_true( length(fn) == 3, label ="3 outfiles expected")

for ( file in fn ) {
	expect_true(file.exists( file) ,label=file )
}
x = getDifferentials( x, grouping, deg.method='wilcox'	)

grouping <- file.path(prefix, 'data', 'SelectionHSPC_time.txt' )

x = userGrouping(x, grouping )


fn = drcPlots2D( x, 'User.group.2')

expect_true( length(fn) == 3, label ="time 3 outfiles expected")

for ( file in fn ) {
	expect_true(file.exists( file) ,label=file )
}

x = getDifferentials( x, grouping, deg.method='wilcox'	)

x= renderReport(x)

collect = list(
	'User.group.2 Statistical Result' = 0,
	'group information table' = 0,
	'User.group.2.Cpp.csv' = 0, 
	'2D DRC DDRtree dim 1,2 . User.group.2 .' = 0,
	'2D DRC DDRtree dim 2,3 . User.group.2 .' = 0,
	'2D DRC DDRtree dim 1,3 . User.group.2 .' = 0,
	'Time.group.4 Statistical Result' = 0,
	'2D DRC DDRtree dim 1,2 . Time.group.4 .' = 0,
	'2D DRC DDRtree dim 2,3 . Time.group.4 .' = 0,
	'2D DRC DDRtree dim 1,3 . Time.group.4 .' = 0
)

expect = list(
	'User.group.2 Statistical Result' = 2,
	'group information table' = 4,
	'User.group.2.Cpp.csv' = 1, 
	'2D DRC DDRtree dim 1,2 . User.group.2 .' = 2,
	'2D DRC DDRtree dim 2,3 . User.group.2 .' = 2,
	'2D DRC DDRtree dim 1,3 . User.group.2 .' = 2,
	'Time.group.4 Statistical Result' = 2,
	'2D DRC DDRtree dim 1,2 . Time.group.4 .' = 4,
	'2D DRC DDRtree dim 2,3 . Time.group.4 .' = 4,
	'2D DRC DDRtree dim 1,3 . Time.group.4 .' = 4
)

expect[[x@outpath]] = 0
collect[[x@outpath]] = 0

ofile = file.path( x@outpath, 'session-log-for-session-cellexalgrouptest.html')

expect_true( file.exists( ofile), label = ofile)

collect = checkFile(collect, ofile, '21321§"!3' ) ## not split the lines!

expect_equal( collect, expect, label="simple test of resulting HTML")

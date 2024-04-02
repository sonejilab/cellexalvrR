context('cellexalGrouping class')

if ( ! expect_true( rmarkdown::pandoc_available() ,label= "pandoc is installed") ){
	skip ( "Pandoc needed - but missing here")
}
Sys.setenv(TEST_MODE = "TRUE")

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

if ( file.exists( file.path(x@outpath,'cellexalGroupTest')  )){
	unlink( file.path(x@outpath,'cellexalGroupTest'), recursive=TRUE)
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
	'User.group.1 Statistical Result' = 0,
	'group information table' = 0,
	'User.group.1.Cpp.csv' = 0, 
	'2D DRC DDRtree dim 1,2 . User.group.1 .' = 0,
	'2D DRC DDRtree dim 2,3 . User.group.1 .' = 0,
	'2D DRC DDRtree dim 1,3 . User.group.1 .' = 0,
	'Time.group.3 Statistical Result' = 0,
	'2D DRC DDRtree dim 1,2 . Time.group.3 .' = 0,
	'2D DRC DDRtree dim 2,3 . Time.group.3 .' = 0,
	'2D DRC DDRtree dim 1,3 . Time.group.3 .' = 0
)

expect = list(
	'User.group.1 Statistical Result' = 2,
	'group information table' = 4,
	'User.group.1.Cpp.csv' = 1, 
	'2D DRC DDRtree dim 1,2 . User.group.1 .' = 2,
	'2D DRC DDRtree dim 2,3 . User.group.1 .' = 2,
	'2D DRC DDRtree dim 1,3 . User.group.1 .' = 2,
	'Time.group.3 Statistical Result' = 2,
	'2D DRC DDRtree dim 1,2 . Time.group.3 .' = 4,
	'2D DRC DDRtree dim 2,3 . Time.group.3 .' = 4,
	'2D DRC DDRtree dim 1,3 . Time.group.3 .' = 4
)

expect[[x@outpath]] = 0
collect[[x@outpath]] = 0

ofile = file.path( x@outpath, 'session-log-for-session-cellexalgrouptest.html')

expect_true( file.exists( ofile), label = ofile)

collect = checkFile(collect, ofile, '21321§"!3' ) ## not split the lines!

#browser()
#rbind(collect, expect)

expect_equal( collect, expect, label="simple test of resulting HTML")

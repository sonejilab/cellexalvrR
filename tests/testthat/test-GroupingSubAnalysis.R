context('GroupingSubAnalysis')

#prefix = 'tests/testthat'
prefix = '.'

cellexalObj = check(cellexalObj)

opath = file.path( prefix, 'data', 'output', 'GroupingSubAnalysis' )

if ( file.exists( opath )){
	unlink(opath, recursive=TRUE)
}
dir.create( opath )
cellexalObj@outpath = opath

selection1 = file.path( prefix, 'data', 'selection0.txt' )

cellexalObj=  userGrouping( cellexalObj, 'LTHSC_broad')

cellexalObj = sessionPath(cellexalObj, "GroupingTest")

cellexalObj = userGrouping( cellexalObj, selection1 )

compareGeneClusters(
	groupingInfo( cellexalObj, 'User.group.2'), 
	groupingInfo( cellexalObj, 'LTHSC_broad'), 
	cellexalObj,
	altGroupNames=c("VRselection",'LTHSC_broad') 
)

cellexalObj = renderReport( cellexalObj )


source( 'function.R' )

collect = list(

	"Here all groups from the main clustering VRselection are subset based on the clustering LTHSC_broad" = 0,
	"3 genes have been detected in 3 statistical comparisons" = 0,
	"80 genes have been detected in 2 statistical comparisons" = 0,
	"581 genes have been detected in 1 statistical comparisons" = 0
)

expect = list(

	"Here all groups from the main clustering VRselection are subset based on the clustering LTHSC_broad" = 1,
	"3 genes have been detected in 3 statistical comparisons" = 1,
	"80 genes have been detected in 2 statistical comparisons" = 1,
	"581 genes have been detected in 1 statistical comparisons" = 1

)

collect = checkFile( collect, file.path(opath, "session-log-for-session-groupingtest.html") )

expect_equal( collect, expect, label="html outfile contents")
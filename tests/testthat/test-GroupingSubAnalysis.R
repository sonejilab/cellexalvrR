context('GroupingSubAnalysis')

if ( ! expect_true( rmarkdown::pandoc_available() ,label= "pandoc is installed") ){
	skip ( "Pandoc needed - but missing here")
}

#prefix = 'tests/testthat'
prefix = '.'

cellexalObj = check(cellexalObj)

opath = file.path( prefix, 'data', 'output', 'GroupingSubAnalysis' )

if ( file.exists( opath )){
	unlink(opath, recursive=TRUE)
}
dir.create( opath )
cellexalObj = reset(cellexalObj)

cellexalObj@outpath = opath

selection1 = file.path( prefix, 'data', 'selection0.txt' )

cellexalObj=  userGrouping( cellexalObj, 'LTHSC_broad')

cellexalObj = sessionPath(cellexalObj, "GroupingTest")

cellexalObj = userGrouping( cellexalObj, selection1 )


compareGeneClusters(
	groupingInfo( cellexalObj, cellexalObj@usedObj$lastGroup), 
	groupingInfo( cellexalObj, 'LTHSC_broad'), 
	cellexalObj,
	altGroupNames=c("VRselection",'LTHSC_broad') 
)

cellexalObj = renderReport( cellexalObj )


source( 'function.R' )

collect = list(

	"Here all groups from the main clustering VRselection are subset based on the clustering LTHSC_broad" = 0#,
#	"7 genes have been detected in 3 statistical comparisons" = 0,
	#"42 genes have been detected in 2 statistical comparisons" = 0,
	#"645 genes have been detected in 1 statistical comparisons" = 0
)

expect = list(

	"Here all groups from the main clustering VRselection are subset based on the clustering LTHSC_broad" = 1#,
	#"7 genes have been detected in 3 statistical comparisons" = 1,
	#"42 genes have been detected in 2 statistical comparisons" = 1,
	#"645 genes have been detected in 1 statistical comparisons" = 1

)

collect = checkFile( collect, file.path(opath, "session-log-for-session-groupingtest.html") )

expect_equal( collect, expect, label="html outfile contents")
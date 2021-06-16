context('timeline-compareGeneClusters')


## This is a little more complictaed here.
## What the function expects two reported timelines (including gene clusters)
## but these gene clusters being based on the same genes.
## Both timelines are used at the same time here.


#prefix = 'tests/testthat'
prefix = './'

#genes <- file.path(prefix, 'data/heatmap_0.txt')

checkFile = function ( collect, ofile ) {

  if ( ! file.exists( ofile) ) {
    stop(paste( "the outfile", ofile,"does not exist and can therefore not be examined."))
  }
	con = file(ofile, "r")
	while ( TRUE ) {
  	line = readLines(con, n = 1)
 	if ( length(line) == 0 ) {
   		break
  	}
  	for ( na in names(collect) ){
  		if ( length(grep( na, line))> 0){
  			collect[[na]] = collect[[na]] +1
  		}
  	}
	}
	close(con)

	collect
}


#cellexalObj <- loadObject(file.path(prefix,'data','cellexalObjOK.RData') )

x = cellexalObj
x = reset(x)
x@outpath = file.path(prefix,'data','output','timeLineTest' )

x = sessionPath( x, 'timeSession_CompareGeneLists')

expect_true( x@usedObj$sessionName == 'timeSession_CompareGeneLists',  label='session path not set correctly')

x = userGrouping( x, file.path(prefix, 'data', 'SelectionHSPC_time.txt' ))

x = getDifferentials( x,cellidfile='User.group.1', deg.method= 'wilcox' , Log=TRUE)

expect_equal( names(x@usedObj$timelines), c("lastEntry", "Time.group.2" ),
 label="correct time names")

## get the original timeline:
bossTime = x@usedObj$timelines[[1]] ## latest
subset1 = rownames(x@usedObj$timelines[["lastEntry"]]@dat)[seq(1,nrow(x@usedObj$timelines[["lastEntry"]]@dat),2)]
timeSubset1 = subsetTime( x@usedObj$timelines[["lastEntry"]], subset1)
subset2 = rownames(x@usedObj$timelines[["lastEntry"]]@dat)[seq(2,nrow(x@usedObj$timelines[["lastEntry"]]@dat),2)]
timeSubset2 = subsetTime( x@usedObj$timelines[["lastEntry"]], subset2)

x= addSelection( timeSubset1, x )
timeSubset1 =  x@usedObj$timelines[[1]]
x= createStats( timeSubset1, x)
ret = createReport(timeSubset1, x, groupingInfo( x,timeSubset1@gname ))
x = ret$cellexalObj
timeSubset1 = ret$timeline

x= addSelection( timeSubset2, x )
timeSubset2 =  x@usedObj$timelines[[1]]
x= createStats( timeSubset2, x)
ret = createReport(timeSubset2, x, groupingInfo( x,timeSubset2@gname ))
x = ret$cellexalObj
timeSubset2 = ret$timeline


x = compareGeneClusters ( timeSubset1, timeSubset2, x, altGroupNames=c("A", "B" ) )


ofile = file.path( x@outpath, 'session-log-for-session-timesession-comparegenelists.html')

if ( file.exists( ofile)) {
	unlink(ofile)
}
x = renderReport( x) 

expect_true( file.exists(ofile) , label="Main outfile produced")

collect= list(
  '2D DRC DDRtree dim 1,2 ( Time.group.2 )' = 0,
  '2D DRC DDRtree dim 2,3 ( Time.group.2 )' = 0,
  'TimeLine control from Saved Selection 1' = 0,
  'Gene group 6' = 0,
  'Statistical result from Time.group.3' = 0,
  '2D DRC DDRtree dim 1,2 ( Time.group.3 )' = 0,
  '2D DRC DDRtree dim 2,3 ( Time.group.3 )' = 0,
  'Comparison between the gene clusters of timeline Time.group.3 or A and timeline Time.group.4 or B' = 0,
  'The following genes from timeline Time.group.3 or A cluster 1 have ended up in the timeline Time.group.4 or B cluster 3' = 0,
  'Genes expression in Time.group.3 or A' = 0, 
  'Genes expression in Time.group.4 or B' = 0
  )


collect = checkFile( collect, ofile)



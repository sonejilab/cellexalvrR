context('linearSelection-compareGeneClusters')

if ( ! expect_true( rmarkdown::pandoc_available() ,label= "pandoc is installed") ){
  skip ( "Pandoc needed - but missing here")
}

## This is a little more complictaed here.
## What the function expects two reported linearSelections (including gene clusters)
## but these gene clusters being based on the same genes.
## Both linearSelections are used at the same time here.


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

expect_equal( names(x@usedObj$linearSelections), c("lastEntry", "Time.group.2" ),
 label="correct time names")

## get the original linearSelection:
bossLinearSelection = x@usedObj$linearSelections[[1]] ## latest
subset1 = rownames(x@usedObj$linearSelections[["lastEntry"]]@dat)[seq(1,nrow(x@usedObj$linearSelections[["lastEntry"]]@dat),2)]
linearSubset1 = subset( x@usedObj$linearSelections[["lastEntry"]], subset1)
subset2 = rownames(x@usedObj$linearSelections[["lastEntry"]]@dat)[seq(2,nrow(x@usedObj$linearSelections[["lastEntry"]]@dat),2)]
linearSubset2 = subset( x@usedObj$linearSelections[["lastEntry"]], subset2)

deg.genes = x@usedObj$deg.genes

x= addSelection( linearSubset1, x )
linearSubset1 =  x@usedObj$linearSelections[[1]]
x= createStats( linearSubset1, x)
deg.genes = c( deg.genes, x@usedObj$deg.genes)

x= addSelection( linearSubset2, x )
linearSubset2 =  x@usedObj$linearSelections[[1]]
x= createStats( linearSubset2, x)
deg.genes = c( deg.genes, x@usedObj$deg.genes)

deg.genes=unique(deg.genes)

ret = createReport(linearSubset1, x, groupingInfo( x,linearSubset1@gname ), deg.genes=deg.genes )
x = ret$cellexalObj
linearSubset1 = ret$linearSelection

ret = createReport(linearSubset2, x, groupingInfo( x,linearSubset2@gname ), deg.genes=deg.genes )
x = ret$cellexalObj
linearSubset2 = ret$linearSelection


x = compareGeneClusters ( linearSubset1@gname, linearSubset2@gname, x, altGroupNames=c("A", "B" ) )


## depending if on linux or windows I get different outfiles ?!

ofile = c( 
  file.path( x@outpath, 'session-log-for-session-timesession-comparegenelists.html'),
  file.path( x@outpath, 'session-log-for-session-timesession_comparegenelists.html')
)

if ( file.exists( ofile[1])) {
	unlink(ofile[1])
}
if ( file.exists( ofile[2])) {
  unlink(ofile[2])
}

x = renderReport( x) 

expect_true( any ( c(file.exists(ofile[1]), file.exists(ofile[2]))) , label="Main outfile produced")

collect= list(
  '2D DRC DDRtree dim 1,2 . Time.group.2 .' = 0,
  '2D DRC DDRtree dim 1,3 . Time.group.2 .'  =0,
  '2D DRC DDRtree dim 2,3 . Time.group.2 .' = 0,
  'Time.group.2 Statistical Result' = 0,
  'Time.group.2 TimeLine control' = 0,
  'Gene group 1' = 0,
  'Gene group 2' = 0, 
  'Gene group 3' = 0,
  'Gene group 4' = 0,
  'Gene group 5' = 0,
  'Gene group 6' = 0,
  'Time.group.3 Statistical Result' = 0,
  'Time.group.3 TimeLine control' = 0,
  '2D DRC DDRtree dim 1,2 . Time.group.3 .' = 0,
  '2D DRC DDRtree dim 1,3 . Time.group.3 .'  =0,
  '2D DRC DDRtree dim 2,3 . Time.group.3 .' = 0,
  'Expression in A .Time.group.3. and trajectory' = 0,
  'Expression in B .Time.group.4. and trajectory' = 0,
  'Click to expand gene list' = 0
)

if ( file.exists( ofile[1])) {
  collect = checkFile( collect, ofile[1])
}else{
  collect = checkFile( collect, ofile[2])
}

#browser()
expect= list(
  '2D DRC DDRtree dim 1,2 . Time.group.2 .' = 4,
  '2D DRC DDRtree dim 1,3 . Time.group.2 .'  =4,
  '2D DRC DDRtree dim 2,3 . Time.group.2 .' = 4,
  'Time.group.2 Statistical Result' = 2,
  'Time.group.2 TimeLine control' = 2,
  'Gene group 1' = 3,
  'Gene group 2' = 3, 
  'Gene group 3' = 3,
  'Gene group 4' = 3,
  'Gene group 5' = 3,
  'Gene group 6' = 3,
  'Time.group.3 Statistical Result' = 2,
  'Time.group.3 TimeLine control' = 2,
  '2D DRC DDRtree dim 1,2 . Time.group.3 .' = 4,
  '2D DRC DDRtree dim 1,3 . Time.group.3 .'  =4,
  '2D DRC DDRtree dim 2,3 . Time.group.3 .' = 4,
  'Expression in A .Time.group.3. and trajectory' = 10,
  'Expression in B .Time.group.4. and trajectory' = 10,
  'Click to expand gene list' = 18
)

#browser()
#rbind( expect, collect)

expect_equal( collect, expect, label="html internals")



context('linearSelection - subsets')

#skip('not stable enough')

#prefix = 'tests/testthat'
prefix = './'

#genes <- file.path(prefix, 'data/heatmap_0.txt')

checkFile = function ( collect, ofile ) {

	con = file(ofile, "r")
	while ( TRUE ) {
  	line = readLines(con, n = 1)
 	if ( length(line) == 0 ) {
   		break
  	}
  	for ( na in names(collect) ){
  		if ( length(grep( na, line))> 0){
  			collect[[na]] = collect[[na]] + length(grep( na, unlist(stringr::str_split(line, "a>"))))
  		}
  	}
	}
	close(con)

	collect
}

#genes = read.delim(genes)[,1]

#cellexalObj <- loadObject(file.path(prefix,'data','cellexalObjOK.RData') )

x = cellexalObj

x@outpath = file.path(prefix,'data','output','timeLineTest' )

if ( !file.exists(x@outpath)){
	dir.create( x@outpath )
}

x = reset(x)
x@outpath = file.path(prefix,'data','output','timeLineTest' )


x = sessionPath( x, 'timeSession_subsets')
expect_true( x@usedObj$sessionName == 'timeSession_subsets',  label='session path not set correctly')

ofile = file.path( prefix, 'data','output','timeLineTest', 'timeSession_subsets', 'AA_Start_paritalLog.Rmd' )
expect_true( file.exists( ofile), label="start session optupt is missing")


ofile = file.path( prefix, 'data','output','timeLineTest', 'timeSession_subsets', 'AB_OneGroupTime_paritalLog.Rmd' )
if ( file.exists( ofile) ) {
	unlink( ofile )
}


gFile= 'SelectionHSPC_time.txt'
grouping <- file.path(prefix, 'data', gFile )


## I need the 3D vectors for the cells in e.g. group 1

x = userGrouping( x, grouping)

x = getDifferentials( x,cellidfile='User.group.1', deg.method= 'wilcox' , Log=TRUE)

expect_equal( names(x@usedObj$linearSelections), c("lastEntry", "Time.group.2" ), label="correct time names")

expect_equal(names(x@usedObj$linearSelections[["Time.group.2"]]@geneClusters[["User.group.1"]][['clusters']]),
			c("1","2","3","4","5",'6'), label = "geneClusters are part of the linearSelections" )


subset = rownames(x@usedObj$linearSelections[["lastEntry"]]@dat)[seq(1,nrow(x@usedObj$linearSelections[["lastEntry"]]@dat),2)]
linearSubset = subset( x@usedObj$linearSelections[["lastEntry"]], subset)


x = linearAnalysisSubset( linearSubset, x )


expect_equal( names(x@usedObj$linearSelections), c("lastEntry", "Time.group.2", "Time.group.3" ), label="correct time names #2")

expect_equal(names(x@usedObj$linearSelections[["Time.group.3"]]@geneClusters[["Time.group.2"]][['clusters']]),
			c("1","2","3","4","5","6"), label = "geneClusters are part of the linearSelections #3" )


x= renderReport( x )

ofile = file.path( prefix, 'data','output', 'timeLineTest', 
	'session-log-for-session-timesession-subsets.html' )
expect_true( file.exists( ofile), label= ofile )

## check the html file for duplicated entries (sections).

collect = list( 
	'Time.group.2 TimeLine control' = 0,
	'as group Time.group.2' = 0,
	'Time.group.2.Linear.csv' = 0,
	'Session Log for Session timeSession_subsets' = 0,
	'2D DRC DDRtree dim 2,3' = 0,
	'href="https://www.genecards.org/cgi-bin/' = 0
)
expt = list( 
	'Time.group.2 TimeLine control' = 4, # one in the text and one in the TOC
	'as group Time.group.2' = 2,
	'Time.group.2.Linear.csv' = 2,
	'Session Log for Session timeSession_subsets' = 2, # one in the text and one in the TOC
	'2D DRC DDRtree dim 2,3' = 8, # this figure is shown twice..
	'href="https://www.genecards.org/cgi-bin/' = 1000 # two 250 gene lists times two (heatmap and lexical order)
)
expt[[x@outpath]] = 0
collect[[x@outpath]] = 0


collect = checkFile(collect, ofile )

expect_equal( collect, expt, label="No duplicate entries in the HTMP file")
##################################################
context('linearSelection check if data is correct')
##################################################

x = check( x ) 
expect_true( x@usedObj$checkPassed, label = x@usedObj$checkError )
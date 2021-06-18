context('timeline - only one')

#skip('not stable enough')

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
  			collect[[na]] = collect[[na]] +1
  		}
  	}
	}
	close(con)

	collect
}

#genes = utils::read.delim(genes)[,1]

#cellexalObj <- loadObject(file.path(prefix,'data','cellexalObjOK.RData') )

x = cellexalObj

x@usedObj$gene_clusters = 10

x@outpath = file.path(prefix,'data','output','timeLineTest2' )

if ( file.exists(x@outpath ) ){
	unlink( x@outpath ,recursive=TRUE)
}
dir.create( x@outpath )

x = reset(x)
x@outpath = file.path(prefix,'data','output','timeLineTest2' )

x = sessionPath( x, 'timeSession_only_one')

ofile = file.path( prefix, 'data','output','timeLineTest2', 'timeSession', 'AB_OneGroupTime_paritalLog.Rmd' )
if ( file.exists( ofile) ) {
	unlink( ofile )
}


expect_true( x@usedObj$sessionName == 'timeSession_only_one',  label='session path not set correctly')
gFile= 'SelectionHSPC_time.txt'
grouping <- file.path(prefix, 'data', gFile )


## I need the 3D vectors for the cells in e.g. group 1

x = userGrouping( x, grouping)

old = dim (x@data)
x = getDifferentials( x,'User.group.1', deg.method= 'wilcox' , Log=TRUE)
new = dim(x@data)
expect_equal( old, new, label="cell or genes lost during getDifferentials")

x= renderReport( x )

ofile = file.path( prefix, 'data','output', 'timeLineTest2', 'session-log-for-session-timesession-only-one.html' )
expect_true( file.exists( ofile), label= ofile )

## check the html file for duplicated entries (sections).

collect = list( 
	'Time.group.2 TimeLine control' = 0,
	'as group Time.group.2' = 0,
	'Time.group.2.Linear.csv' = 0,
	'Session Log for Session timeSession_only_one' = 0,
	'2D DRC DDRtree dim 2,3' = 0
)

expt = list( 
	'Time.group.2 TimeLine control' = 2, # one in the text and one in the TOC
	'as group Time.group.2' = 1,
	'Time.group.2.Linear.csv' = 1,
	'Session Log for Session timeSession_only_one' = 2, # one in the text and one in the TOC
	'2D DRC DDRtree dim 2,3' = 4 # this figure is shown twice..
)
expt[[x@outpath]] = 0
collect[[x@outpath]] = 0
collect = checkFile(collect, ofile )

expect_equal( collect, expt, label="No duplicate entries in the HTMP file")

##################################################
context('timeline check if data is correct')
##################################################

x = check( x ) 
expect_true( x@usedObj$checkPassed, label = x@usedObj$checkError )
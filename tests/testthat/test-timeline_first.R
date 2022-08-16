context('timeline')

#skip('not stable enough')

prefix = './'

#genes <- file.path(prefix, 'data/heatmap_0.txt')

#genes = utils::read.delim(genes)[,1]

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

#cellexalObj <- loadObject(file.path(prefix,'data','cellexalObjOK.RData') )

x = cellexalObj

x@outpath = file.path(prefix,'data','output','timeLineTest' )

if ( file.exists(x@outpath ) ){
	unlink( x@outpath ,recursive=TRUE)
}
dir.create( x@outpath )

x = reset(x)
x@outpath = file.path(prefix,'data','output','timeLineTest' )

x = sessionPath( x, 'timeSession')

ofile = file.path( prefix, 'data','output','timeLineTest', 'timeSession', 'AB_OneGroupTime_paritalLog.Rmd' )
if ( file.exists( ofile) ) {
	unlink( ofile )
}


expect_true( x@usedObj$sessionName == 'timeSession',  label='session path not set correctly')
gFile= 'SelectionHSPC_time.txt'
grouping <- file.path(prefix, 'data', gFile )


## I need the 3D vectors for the cells in e.g. group 1



x = userGrouping( x, grouping)

t = reduceTo( x, what='col', 'to'= 	colnames(x@data)[which( x@userGroups[,x@usedObj$lastGroup] == 1 )] )

#t= pseudotimeTest3D( t, dat[,1], dat[,2], dat[,3], x@usedObj$lastGroup )

ofiles = c ('AB_Stats_timeSession.html' ,'AC_OneGroupTime_timeSession.html')
SelectionFile = file.path(x@outpath , paste(gFile, 'time', sep=".") )

for ( ofile in ofiles){
	ofile = file.path(x@outpath, ofile)
	if ( file.exists( ofile) ) {
		unlink( ofile )
	}
}
if ( file.exists( SelectionFile)){
	unlink( SelectionFile )
}
t = getDifferentials( t,'User.group.1', deg.method= 'wilcox' , Log=FALSE)


for ( ofile in ofiles){
	ofile = file.path(t@outpath, ofile)
	expect_true(file.exists( ofile), label=ofile  )
}

time= t@usedObj$timelines[['lastEntry']]
o = order(time@dat$time)
d = utils::read.delim(SelectionFile, header=F )

#expect_true( all.equal( as.vector(d[,1]), names(time$c)[o]) == TRUE, "new order was wrong")

#expect_true( all.equal( as.vector(d[,2]), gplots::bluered( 9 ))==TRUE, "new order")



expect_true( file.exists( ofile), label= ofile )
expect_true( file.exists( SelectionFile), label= SelectionFile)

ofiles = c ('AD_Stats_timeSession.html' ,'AE_OneGroupTime_timeSession.html')
SelectionFile = file.path(x@outpath , paste(gFile, 'time', sep=".") )

for ( ofile in ofiles){
	ofile = file.path(x@outpath, ofile)
	if ( file.exists( ofile) ) {
		unlink( ofile )
	}
}
if ( file.exists( SelectionFile)){
	unlink( SelectionFile )
}

a = getDifferentials( x,'User.group.1' ,deg.method= 'wilcox' , Log=FALSE)
Sys.sleep(2)

for ( ofile in ofiles){
	ofile = file.path(x@outpath, ofile)
	expect_true( file.exists( ofile), label= ofile )
}
expect_true( file.exists( SelectionFile), label= SelectionFile )
t = table(utils::read.delim( SelectionFile, header=F )[,2])

expect_true(length(t) <= 10,  label=paste("same time colors (", sep="", length(t)," > 10)" ) )

####################################################
context('timeline reproducibility')
####################################################

## the output from the time process is the file SelectionHSPC_time.txt.time no higher time resolution available
## and this function is reproducible if I get the same thing multiple times - right?

testF = file.path( prefix, 'data','output','timeLineTest','SelectionHSPC_time.txt.time' )
cmpFile = file.path( prefix, 'data','output','SelectionHSPC_time.txt.time' )
if ( ! file.exists( cmpFile )){
	file.copy( testF, cmpFile )
}


old = utils::read.delim( cmpFile )
new = utils::read.delim( testF )

expect_true( all.equal( old[,1], new[,1] ) ==TRUE, label= "old and new 1 are the same" )
expect_true( all.equal( old[,3], new[,3] ) ==TRUE, label= "old and new 3 are the same")



ofile = file.path( prefix, 'data','output','timeLineTest',
	'AG_OneGroupTime_timeSession.html' )
if ( file.exists( ofile) ) {
	unlink( ofile )
}
if ( file.exists( SelectionFile)){
	unlink( SelectionFile )
}

a = getDifferentials( x,'User.group.1' ,deg.method= 'wilcox' , Log=FALSE)
Sys.sleep(2)

expect_true( file.exists( ofile), label= ofile )
expect_true( file.exists( SelectionFile), label= SelectionFile )

new = utils::read.delim( testF )

expect_true( all.equal( old[,1], new[,1] ) ==TRUE )
expect_true( all.equal( old[,3], new[,3] ) ==TRUE )

x= renderReport( x )

ofile = file.path( prefix, 'data','output', 'timeLineTest', 'session-log-for-session-timesession.html' )
expect_true( file.exists( ofile), label= ofile )

collect= list( 
)
expt = list( 
)
expt[[x@outpath]] = 0
collect[[x@outpath]] = 0

collect = checkFile ( collect, ofile )
expect_equal( collect, expt, label="No cellexalvrR outpath HTMP file")


##################################################
context('timeline check if data is correct')
##################################################

x = check( x ) 
expect_true( x@usedObj$checkPassed, label = x@usedObj$checkError )


context('timeline')

skip('not stable enough')

prefix = './'

#genes <- file.path(prefix, 'data/heatmap_0.txt')

#genes = read.delim(genes)[,1]

cellexalObj <- loadObject(file.path(prefix,'data','cellexalObjOK.RData') )

x = cellexalObj

x@outpath = file.path(prefix,'data','output','timeLineTest' )

x@usedObj$sessionPath = x@usedObj$sessionRmdFiles = x@usedObj$sessionName = NULL

ofile = file.path( prefix, 'data','output','timeLineTest', 'timeSession', '1_OneGroupTime_paritalLog.Rmd' )
if ( file.exists( ofile) ) {
	unlink( ofile )
}
x = sessionPath( x, 'timeSession')

expect_true( x@usedObj$sessionName == 'timeSession', 'session path not set correctly')

grouping <- file.path(prefix, 'data/selection0.txt')

## I need the 3D vectors for the cells in e.g. group 1
x = userGrouping( x, grouping)
#which( x@userGroups[,x@usedObj$lastGroup] == 1 )
x@userGroups[,'User.group.2'] = NA
x@userGroups[,'User.group.2.order'] = x@userGroups[,'User.group.1.order']
x@userGroups[which(x@userGroups[,'User.group.1'] == 2), 'User.group.2'] = 1


x@groupSelectedFrom[['User.group.2']] = x@groupSelectedFrom[['User.group.1']] 


dat = x@drc[['DDRtree']][which( x@userGroups[,x@usedObj$lastGroup] == 2 ), ]

t = reduceTo( x, what='col', 'to'= colnames(x@data)[which( x@userGroups[,x@usedObj$lastGroup] == 2 )] )

#t= pseudotimeTest3D( t, dat[,1], dat[,2], dat[,3], x@usedObj$lastGroup )
ofile = file.path( prefix, 'data','output','timeLineTest','2_OneGroupTime_timeSession.html' )
if ( file.exists( ofile) ) {
	unlink( ofile )
}

t = getDifferentials( t,'User.group.2' ,deg.method= 'wilcox' , Log=FALSE)

expect_true( file.exists( ofile), "Rmd (subset) ofile not created" )
if ( file.exists( ofile) ) {
	unlink( ofile )
}

a = getDifferentials( x,'User.group.2' ,deg.method= 'wilcox' , Log=FALSE)
ofile = file.path( prefix, 'data','output','timeLineTest','3_OneGroupTime_timeSession.html' )

expect_true( file.exists( ofile), "Rmd (total) ofile not created" )

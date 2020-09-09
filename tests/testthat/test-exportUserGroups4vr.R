context('exportUserGroups4vr')

## this is an in depth check of internal logics.

prefix = './'

data = file.path(prefix, 'data/cellexalObj.RData')

cellexalObj = loadObject( data )

cellexalObj@userGroups=data.frame()
cellexalObj@usedObj$lastGroup = NULL
cellexalObj@usedObj$SelectionFiles = list()

datadir <- normalizePath(file.path( prefix, 'data', 'output'))
cellexalObj@usedObj$sessionPath = cellexalObj@usedObj$sessionRmdFiles = cellexalObj@usedObj$sessionName = NULL

cellexalObj@outpath = file.path(datadir)

grouping =  file.path(prefix, 'data', 'selection0.txt' )

cellexalObj = userGrouping( cellexalObj, grouping )

if ( file.exists( file.path(datadir, 'User.group.1.cgr' ))){
	unlink( file.path(datadir, 'User.group.1.cgr' ) )
}
exportUserGroups4vr(cellexalObj, datadir )


old = read.delim( grouping, header=F, row.names=1 )
new = read.delim(file.path(datadir, 'User.group.1.cgr' ), header=F, row.names=1 )

m = match(rownames(old),rownames(new))

expect_equal(old, new[m,], info="exportUserGroups4vr reproduces selection file" )
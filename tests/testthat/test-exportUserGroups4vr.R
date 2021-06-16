context('exportUserGroups4vr')

## this is an in depth check of internal logics.

prefix = './'


datadir <- file.path( prefix, 'data', 'output','exportUserGroups4vr')
if ( file.exists( datadir )){
	unlink(datadir, recursive=TRUE)
}
dir.create(datadir)

cellexalObj=reset ( cellexalObj )

cellexalObj@outpath = file.path(datadir)

grouping =  file.path(prefix, 'data', 'selection0.txt' )

cellexalObj = userGrouping( cellexalObj, grouping )

if ( file.exists( file.path(datadir, 'User.group.1.cgr' ))){
	unlink( file.path(datadir, 'User.group.1.cgr' ) )
}
exportUserGroups4vr(cellexalObj, datadir )


old = utils::read.delim( grouping, header=F, row.names=1 )
new = utils::read.delim(file.path(datadir, 'User.group.1.cgr' ), header=F, row.names=1 )

m = match(rownames(old),rownames(new))

expect_equal(old, new[m,], info="exportUserGroups4vr reproduces selection file" )
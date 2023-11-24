context('createGrouping')

x = cellexalObj

prefix = './'

x = reset(x)
x@outpath = file.path(prefix,'data','output','subset' )
x = sessionPath( x, 'test')

selectionFile =file.path(prefix, 'data', 'SelectionHSPC_time.txt' )

old = userGrouping( x, selectionFile)
session_sel_file = file.path(prefix,'data','output','subset','test', 'SelectionHSPC_time.txt' )
if ( file.exists( session_sel_file ) ){
	unlink( session_sel_file )
}

data = read.delim( file=selectionFile, header=F )

grouping = data[,4]
cells = data[,1]
drc = data[1,3]

uni_g = unique(grouping)
first_group_cell_pos = match( uni_g, grouping)
col =  data[1,2]

new= createGrouping ( x, cells = cells, groups = grouping +1, 
	drc = drc, col= col, cellidfile=selectionFile  ) 

expect_true( file.exists( session_sel_file ), "selection file has been copied")

expect_equal ( old@groupSelectedFrom[[1]], new@groupSelectedFrom[[1]], info= "the selection objects are the same" )

old@usedObj$SelectionFiles = new@usedObj$SelectionFiles

expect_equal ( old, new, info= "the two cellexalObjecs are the same" )

context('createGrouping')

x = cellexalObj

prefix = './'

x = reset(x)
x@outpath = file.path(prefix,'data','output','subset' )
x = sessionPath( x, 'test')

cellidfile = file.path(prefix, 'data', 'SelectionHSPC_time.txt' )

cellid <- utils::read.delim(cellidfile,header=F)
if ( cellid[1,1] == 'CellID'){ ## reading a file from the session path
	cellid = cellid[-1,]
}

groups = cell_id[, 4]
uniq_gr = unique( groups)
col_pos= match( uniq_gr, cellid[,4])
col = split( cellid[col_pos,2], cellid[col_pos,4] )

expect_equal( 
	createGrouping ( x, cells = cellid[,1], groups=cellid[,4], 
	drc = cellid[2,1], cellidfile = cellidfile,  col=col ),
	userGrouping( x, cellidfile ) )

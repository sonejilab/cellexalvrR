context('addSelction')

skip("Not really necessary" )
x = cellexalObj

prefix = './'

x = reset(x)
x@outpath = file.path(prefix,'data','output','subset' )
x = sessionPath( x, 'test')

selectionFile =file.path(prefix, 'data', 'SelectionHSPC_time.txt' )

old = userGrouping( x, selectionFile)

data = read.delim( file=selectionFile, header=F )


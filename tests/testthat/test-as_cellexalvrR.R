context('import from h5ad files (scanpy)')

#prefix = 'tests/testthat'
Sys.setenv(TEST_MODE = "TRUE")
prefix = '.'

ipath = file.path( prefix, 'data' )

data = as_cellexalvrR( file.path( ipath, 'tiny2.h5ad'), 
	meta.cell.groups=c( 'louvain'), specie='mouse' )
exp = table( c( rep('MSC_O1',2), rep('MSC_O2', 50), rep('Y1', 344)) )

expect_equal( table(data@usedObj$samples[,'sname']), exp, label="categories copied here as strings" )

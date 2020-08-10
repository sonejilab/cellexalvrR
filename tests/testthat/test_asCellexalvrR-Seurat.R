context('import from Seurat V 3.x')

skip_if_not ( require('Seurat', q=T), 'Seurat package is not installed' )

ret = as_cellexalvrR( 
    'x' = pbmc_small,  
    'meta.cell.groups' = c('groups'), 
    'specie' = 'mouse' 
)

expect_equal(dim(ret@data), c(230, 80) )

expect_equal( dim(ret@meta.cell), c(80,2) )

expect_equal( dim(ret@meta.gene), c(230,1) )

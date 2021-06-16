context('get.genmes.cor.to function')

prefix = './'

genes <- file.path(prefix, 'data/heatmap_0.txt')

genes = utils::read.delim(genes)[,1]


if ( file.exists( file.path(prefix, 'function.R'))){
	## the test functions that should not be maintained in differet test scripts...
	source( file.path(prefix, 'function.R')) 

}

#cellexalObj <- loadObject(file.path('data','cellexalObj.RData') )

x = reset(cellexalObj)

x@outpath =  file.path(prefix, 'data', 'output', 'GeneGeneCorrelations')

x= sessionPath(x, 'test' )

ofile= 'Procr_correlated_genes.txt' 

tab = get.genes.cor.to (x, gname = 'Procr', output=  ofile, is.smarker = F, cpp=T )

tab1 = get.genes.cor.to (x, gname = 'Procr', output=  ofile, is.smarker = F, cpp=F )

expect_equal( tab, tab1, "c++ same outpout as R")

x = renderReport( x )

##expect_true( length( gene3) == 100, paste("wrong gene number seurat wilcox", length( gene3) ) )


ofile = file.path( x@outpath, 'session-log-for-session-test.html' )

expect_true( file.exists( ofile), label="main html report file")

count = list(
	'2D DRC diffusion dim 1,2' = 0,
	'2D DRC diffusion dim 2,3' = 0,
	'2D DRC diffusion dim 1,3' = 0,
	'Genes negatively correlated to Procr' = 0,
	'Genes correlated to Procr' = 0,
	'Genes positively correlated to Procr' = 0,
	'Max correlation value is 0.578 and min correlation value is -0.51' = 0
)
expect = list(
	'2D DRC diffusion dim 1,2' = 8, ## are also in the toc
	'2D DRC diffusion dim 2,3' = 4,
	'2D DRC diffusion dim 1,3' = 4,
	'Genes negatively correlated to Procr' = 4,
	'Genes correlated to Procr' = 4,
	'Genes positively correlated to Procr' = 4,
	'Max correlation value is 0.578 and min correlation value is -0.51' = 2
)
genes = c(tab[,1], tab[,2])
genes = rownames( x@data )[ match( tolower( genes ), tolower( rownames( x@data )) )]

for ( gene in genes ) {
	count[[gene]] = 0
	expect[[gene]] = 4
}
expect[[ 'Mpl' ]] = 18
expect[[ 'Cdk6' ]] = 18


expect_true( file.exists( ofile), label = "main html report file")


count = checkFile( count, ofile, 'kghug')

expect_equal( count, expect, label="HTML file internals")


#test_texpect_true( file.exists( ofile), labels="main html report file")hat( 'MAST' ,{
#	skip_if_not_installed( 'Seurat' )		
#	options(warn=-1)
#	gene1 = getDifferentials(x, grouping, 'MAST', num.sig=100, Log=FALSE )
#	options(warn=1)
#	expect_true( length( gene1) == 100,  paste("wrong gene number MAST", length( gene1) )  )
#})
#
#test_that( 'poisson' ,{
#	skip_if_not_installed( 'Seurat' )
#	gene1 = getDifferentials(x, grouping, 'poisson', num.sig=100 , Log=FALSE)
#	expect_true( length( gene1) == 100, paste("wrong gene number poisson", length( gene1) )  )
#})

#gene1 = getDifferentials(x, grouping, 'anova', num.sig=100 )




## get a grouping added?

context('sorting')

if ( ! expect_true( rmarkdown::pandoc_available() ,label= "pandoc is installed") ){
	skip ( "Pandoc needed - but missing here")
}

prefix = './'


if ( file.exists( file.path( prefix, 'data', 'output', 'sortingTest') )){
	unlink( file.path( prefix, 'data', 'output', 'sortingTest'), recursive=TRUE)
}
## Horribly simple data structure

set.seed(5)

SIZE = 50

d = rnorm ( SIZE *50 )

geneA = cbind(matrix(d,ncol=SIZE), matrix(d+3, ncol=SIZE))
geneB = cbind(matrix(d+4,ncol=SIZE), matrix(d, ncol=SIZE))

LETTERS = as.vector( sapply(LETTERS, function(x) paste0(x, LETTERS)))


data= rbind( geneA, geneB)
rownames(data) = paste(sep="", 'gene', LETTERS[1:nrow(data)])


firstHalve =  matrix(ncol=3,c( rnorm(3*SIZE) ))
secondHalve = firstHalve
secondHalve[,1] = secondHalve[,1] + max( secondHalve[,1] ) *2
firstHalve = rbind(firstHalve, secondHalve )
secondHalve = firstHalve
secondHalve[,1] = secondHalve[,1] + max( secondHalve[,1] ) *2
drc = rbind(firstHalve, secondHalve )
drc[,2] = drc[,2] / 10
drc[,3] = drc[,3] / 10
drc = drc[sample(1:nrow(drc), SIZE*2),]

colnames(data) = c( paste('TypeA', 1:SIZE), paste("TypeB", 1:SIZE))

rownames(drc) = colnames(data)

samp = data.frame(cbind( 'sampleID' = colnames(data), 
	'sampleType'= c(rep('A',SIZE), rep('B', SIZE)),  
	useless= LETTERS[1:(SIZE*2)], rev=LETTERS[(SIZE*2):1]) )
rownames(samp) = colnames(data)

genes =  as.matrix( data.frame( "Gene Name" = rownames(data), useless = 1:nrow(data)) )
rownames(genes) = genes[,1]

obj = new("cellexalvrR",
	data= Matrix::Matrix(data, sparse=T),
	drc=list('pca'= drc),
	meta.gene = genes,
	meta.cell=make.cell.meta.from.df( samp, colnames(samp)),  
	specie='mouse', outpath= file.path( prefix, 'data', 'output', 'sortingTest')
)

obj=sessionPath( obj, 'test1')

obj = check(obj)

expect_true( obj@usedObj$checkPassed, label="synthetic object creation" )

obj@usedObj$samples = samp

d1 = reorderSamples(obj, 'rev')

d2 = reorderSamples(d1, 'useless')

expect_true( ! isTRUE( all.equal( obj@usedObj$samples, d1@usedObj$samples)), label="sort changed the samples info")
expect_true( isTRUE( all.equal( obj@usedObj$samples, d2@usedObj$samples)), label="sort back got same samples info")

obj@userGroups = d2@userGroups

expect_true( isTRUE( all.equal( obj, d2)), label="sort back got same samples info")

context('sorting - groupings')

obj = userGrouping( obj, 'sampleType')

g0 = groupingInfo( obj,'sampleType')

d1 = reorderSamples(obj, 'rev')
g1 = groupingInfo( d1,'sampleType')

expect_equal(  g0@grouping , rev(g1@grouping), label="GrouingInfo switched" )

f0 = drcPlots2D( obj, 'sampleType')

f1 = drcPlots2D( d1, 'sampleType')

png0 =png::readPNG(f0[1])
png1 =png::readPNG(f1[1])
expect_true( sum(png0 - png1) == 0, label="drc plots are exactly the same for orig and revers")

context('sorting - linear')


obj@userGroups[,'test'] = rep(1, nrow(obj@userGroups) )
obj@userGroups[,'test order'] = 1:nrow(obj@userGroups)

obj = getDifferentials( obj, 'test', log=FALSE, num.sig = 40) ## should get a linear object


time0 = groupingInfo( obj )@linarObj 

d1 = reorderSamples(obj, 'rev')

time1 = groupingInfo( d1 )@linarObj 

expect_equal( time0, time1, label="timelines should not change")

f0 = drcPlots2Dlinear( obj, time0)

f1 = drcPlots2Dlinear( d1, time1)

png0 =png::readPNG(f0[1])
png1 =png::readPNG(f1[1])

expect_true( sum(png0 - png1) == 0, label="drc plots are exactly the same for orig and revers linear selection")

## Strange - I really had expected to find some incoherences here.
## Crap. But better to test than to keep untested..


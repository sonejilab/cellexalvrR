context('sigGeneList')

prefix = './'

# prefix ='tests/testthat'

cellexalObj = reset( cellexalObj)

cellexalObj@outpath = file.path( prefix, 'data/output/sigGeneList' )
if ( dir.exists( cellexalObj@outpath) ){
	file.remove(cellexalObj@outpath, recursive=TRUE )
}
dir.create( cellexalObj@outpath )

cellexalObj = sessionPath( cellexalObj, 'Toppgenes_input_files')


cellexalObj = getDifferentials( cellexalObj, file.path( prefix, 'data/selection0.txt') )

ret = getSigGenes( cellexalObj@usedObj$sigGeneLists$Cpp$User.group.1, 100)

expect_equal( dim(ret), c(300, 2), label="Correct amount" )

sessionPath = cellexalObj@usedObj$sessionPath # will be removed in the next step...

cellexalObj = renderReport( cellexalObj )

expect_true( file.exists(file.path( sessionPath, 'tables', 'User.group.1.toppgeneInput.csv') ), label="ToppGenes input file exists in log folder" )


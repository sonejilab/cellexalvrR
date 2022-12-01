context('stat functions singleCells')


message("You are testing THIS file")
if ( ! expect_true( rmarkdown::pandoc_available() ,label= "pandoc is installed") ){
	skip ( "Pandoc needed - but missing here")
}

prefix = './'

if ( file.exists( file.path(prefix, 'function.R'))){
	## the test functions that should not be maintained in differet test scripts...
	source( file.path(prefix, 'function.R')) 

}

if ( file.exists( file.path(prefix,'data','output','statTest','statTestSingleC' ) )){
	unlink(  file.path(prefix,'data','output','statTest','statTestSingleC' ), recursive=TRUE )
}

x = reset(cellexalObj)
x@outpath = file.path(prefix,'data','output','statTest' )
x = sessionPath( x, 'statTestSingleC' )


if ( ! file.exists(x@outpath)){
	dir.create(x@outpath)
}

grouping <- file.path(prefix, 'data/selection0.txt')

gene2 = getDifferentials(x, grouping, 'wilcox', num.sig=100, Log=FALSE, logfc.threshold = .1, minPct=0.1, bulkStats=FALSE )

expect_true( length(gene2@usedObj$deg.genes) == 102, info = paste("wrong gene number c++ wilcox", length(gene2) ) )

ofile=  file.path( x@outpath, 'AB_Stats_statTestSingleC.html' )
expect_true( file.exists( ofile), label = ofile)


ofile=  file.path( x@outpath, 'session-log-for-session-stattestsinglec.html' )
if ( file.exists( ofile))
	unlink(ofile)

x = renderReport(x)

expect_true( file.exists( ofile), label="stats main outfile")

search = list(
	'User.group.1 Statistical Result' = 0,
	'./statTestSingleC/tables/User.group.1.Cpp.csv' =0,
	"statTestSingleC/png/User.group.1.DDRtree.1_2.png"= 0,
	"statTestSingleC/png/User.group.1.DDRtree.1_3.png"= 0,
	"statTestSingleC/png/User.group.1.DDRtree.2_3.png"= 0,
	'pseudo bulk samples .n.' = 0
)

search = checkFile( search, ofile )

exp= list(
	'User.group.1 Statistical Result' = 2,
	'./statTestSingleC/tables/User.group.1.Cpp.csv' =1,
	"statTestSingleC/png/User.group.1.DDRtree.1_2.png"= 1,
	"statTestSingleC/png/User.group.1.DDRtree.1_3.png"= 1,
	"statTestSingleC/png/User.group.1.DDRtree.2_3.png"= 1,
	'pseudo bulk samples .n.' = 0
)

expect_equal( search, exp, label="html main file internals")

for ( fn in names(search)[3:5]) {
	expect_true(file.exists( file.path(x@outpath, fn) ), label=fn)
}

## stats data is in gene2@usedObj$sigGeneLists$Cpp$"User.group.1"
## lets validate some

## e.g. them: gene2@usedObj$deg.genes
context('stat functions singleCells - rerun stats')

toCheck = gene2@usedObj$sigGeneLists$Cpp$"User.group.1"[
which(!is.na( 
	match(gene2@usedObj$sigGeneLists$Cpp$"User.group.1"[,'gene'], gene2@usedObj$deg.genes)
	)),
]


checkThis = function(id){
	cluster = toCheck[id,'cluster']
	gene = gene2@data[toCheck[id,'gene'],]
	A = which( gene2@userGroups[,'User.group.1'] == cluster )
	Bbad = c( which( is.na ( gene2@userGroups[,'User.group.1'] )  ), A)
	B = (1:ncol(gene2@data))[-Bbad]

	expect_equal( toCheck[id,"p.value"], wilcox.test( gene[A], gene[B], exact=FALSE)$p.value, label=paste("Wilcox result gene", toCheck[id,"gene"]))
	#browser()
}


t = lapply( 1:nrow(toCheck), checkThis )

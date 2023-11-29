context('stat functions pseudoBulk')

if ( ! expect_true( rmarkdown::pandoc_available() ,label= "pandoc is installed") ){
	skip ( "Pandoc needed - but missing here")
}
Sys.setenv(TEST_MODE = "TRUE")

prefix = './'

if ( file.exists( file.path(prefix, 'function.R'))){
	## the test functions that should not be maintained in differet test scripts...
	source( file.path(prefix, 'function.R')) 

}

if ( file.exists( file.path(prefix,'data','output','statTest','statTestPsedoBulk' ) )){
	unlink(  file.path(prefix,'data','output','statTest','statTestPsedoBulk' ), recursive=TRUE )
}

x = reset(cellexalObj)
x@outpath = file.path(prefix,'data','output','statTest' )

if ( file.exists( x@outpath )){
	unlink(x@outpath, recursive=TRUE)
}
dir.create( x@outpath )

x = sessionPath( x, 'statTestPsedoBulk' )


if ( ! file.exists(x@outpath)){
	dir.create(x@outpath)
}

grouping <- file.path(prefix, 'data/selection00.txt')


gene2 = getDifferentials(x, grouping, 'wilcox', num.sig=100, Log=FALSE, logfc.threshold = .1, minPct=0.1, bulkStats=TRUE, nameAdd="test_" )

expect_true( length( gene2@usedObj$deg.genes) == 102, info = paste("wrong gene number c++ wilcox", length( gene2) ) )
#logStatResult ( gene2, gene2@usedObj$sigGeneLists$Cpp[[gene2@usedObj$lastGroup]],
#method= 'wilcox', 'p.value')

ofile=  file.path( x@outpath, 'AB_Stats_statTestPsedoBulk.html' )
expect_true( file.exists( ofile), label = ofile)


ofile=  file.path( x@outpath, 'session-log-for-session-stattestpsedobulk.html' )
if ( file.exists( ofile))
	unlink(ofile)

x = renderReport(x)

expect_true( file.exists( ofile), label="stats main outfile")

search = list(
	'User.group.1 Statistical Result' = 0,
	'statTestPsedoBulk/tables/User.group.1.Cpp.csv' =0,
	'statTestPsedoBulk/tables/User.group.1.toppClusterInput.csv' =0,
	'statTestPsedoBulk/PseudoBulk.User.group.1.cls' = 0,
	'statTestPsedoBulk/PseudoBulk.User.group.1.gct' = 0,
	"statTestPsedoBulk/png/User.group.1.DDRtree.1_2.png"= 0,
	"statTestPsedoBulk/png/User.group.1.DDRtree.1_3.png"= 0,
	"statTestPsedoBulk/png/User.group.1.DDRtree.2_3.png"= 0,
	'pseudo bulk samples .n.' = 0
)

search = checkFile( search, ofile )

exp= list(
	'User.group.1 Statistical Result' = 2,
	'statTestPsedoBulk/tables/User.group.1.Cpp.csv' =1,
	'statTestPsedoBulk/tables/User.group.1.toppClusterInput.csv' =1,
	'statTestPsedoBulk/PseudoBulk.User.group.1.cls' = 0,
	'statTestPsedoBulk/PseudoBulk.User.group.1.gct' = 0,
	"statTestPsedoBulk/png/User.group.1.DDRtree.1_2.png"= 1,
	"statTestPsedoBulk/png/User.group.1.DDRtree.1_3.png"= 1,
	"statTestPsedoBulk/png/User.group.1.DDRtree.2_3.png"= 1,
	'pseudo bulk samples .n.' = 1
)

#rbind( exp, search )



expect_equal( search, exp, label="html main file internals")

Sys.sleep(2)

for ( fn in names(search)[2:7]) {
	expect_true(file.exists( file.path(x@outpath, fn) ), label=fn)
}


context('stat functions pseudoBulk - summing up the cells')

## to check the stats data I need to load the pseudo - bulk object:
## this will load a loc cellexalvrR ovject with 32 'cells'
load( file.path(gene2@usedObj$sessionPath, 'PseudoBulk.User.group.1..RData'))

cmp = reduceTo(gene2, what='col', to=colnames(gene2@data)[which(!is.na(gene2@userGroups[,1]))])

order= names(loc@usedObj$pseudoBulk_groupings)

cmp@usedObj$samples = data.frame( 'wanted_order'= order( match( order, colnames(cmp@data)) ))
cmp = reorderSamples ( cmp, 'wanted_order' )


bad = which( FastWilcoxTest::ColNotZero( t( cmp@data)) == 0)
if ( length(bad) > 0){
	cmp = reduceTo( cmp, what='row', to=rownames(cmp@data)[-bad] )
}

cell2sample = table(cmp@userGroups[,1], loc@usedObj$pseudoBulk_groupings)

for ( id in names(table(loc@userGroups[,1]))) {
	expect_equal( as.vector( cell2sample[id,] > 0 ) , as.vector(loc@userGroups[,1] == id ) , label=paste("id groupings", id))
}

expect_equal( rownames(cmp@data), rownames(loc@data), label="comapcted and full dataset same rownames")

## now I need to check if the data has been compacted OK
checkMean = function (id){
	d = as.vector(unlist(lapply ( split( cmp@data[id,],loc@usedObj$pseudoBulk_groupings ), mean)))
	expect_equal( as.vector(loc@data[id,]), d, label=paste("pseudo bulk for gene", rownames(loc@data)[id]) )
}

t = lapply( 1:20, checkMean ) ## should be enough



context('stat functions pseudoBulk - rerun stats')

toCheck = gene2@usedObj$sigGeneLists$Cpp$"User.group.1"[
which(!is.na( 
	match(gene2@usedObj$sigGeneLists$Cpp$"User.group.1"[,'gene'], gene2@usedObj$deg.genes)
	)),
]

toCheck = toCheck[which(toCheck$p.value > 1e-16),] ## the intern wilcox test doe not get 'better' p values.

checkThis = function(id){
	cluster = toCheck[id,'cluster']
	gene = loc@data[toCheck[id,'gene'],]
	A = which( loc@userGroups[,'User.group.1'] == cluster )
	Bbad = c( which( is.na ( loc@userGroups[,'User.group.1'] )  ), A)
	B = (1:ncol(loc@data))[-Bbad]
	p.value = 1
	try({p.value = wilcox.test( gene[A], gene[B], exact = FALSE )$p.value
	expect_equal( 
		toCheck[id,"p.value"], 
		p.value, 
		label=paste("Wilcox result gene", toCheck[id,"gene"]),
		#tolerance= 0.01

		)
	})
	p.value
}


## the wilcox.test does not give the same results as the cpp version. That is something for another day.

stats_file = file.path( gene2@usedObj$sessionPath, "tables", "User.group.1.Cpp.csv")
expect_true( file.exists(stats_file), label="stats file exists")
stats = read.delim( stats_file , sep=',')
all.equal( grep('test_', stats$cluster), 1:nrow(stats), label = "nameAdd is in the stats table")

exp = gene2@usedObj$sigGeneLists$Cpp$"User.group.1"
#exp[,'cluster'] = as.numeric(exp[,'cluster'])
rownames(exp) = NULL

expect_equal( stats, exp, label="stats table data as expected" )

#t = unlist(lapply( 1:nrow(toCheck), checkThis ))

#browser()
#toCheck = cbind( toCheck, 'R stats' = t)
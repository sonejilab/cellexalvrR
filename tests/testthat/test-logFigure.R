context('log figure integrated')

prefix = './'

# prefix ='tests/testthat'

source( file.path(prefix, 'function.R') )
## loads a function checkFile( path ) that searches for a list of strings and giuves the number of occurances in the file.

## need to start from scratch - why not a little bit heatmap first?

#cellexalObj <- loadObject(file.path(prefix,'data','cellexalObjOK.RData') )

x = reset(cellexalObj)

x@outpath = file.path(prefix,'data','output','LogFigure' )

if ( ! file.exists(x@outpath)){
	dir.create( x@outpath, recursive=T )
}else {
	unlink(  x@outpath, recursive=T )
	dir.create( x@outpath )
}

x = sessionPath( x, 'logFigureTest')

grouping <- file.path(prefix, 'data/selection0.txt')

context('log figure - stats first')

x@userGroups=data.frame()
x@usedObj$lastGroup = NULL
x@usedObj$SelectionFiles = list()

x = getDifferentials(x, grouping, 'wilcox', num.sig=100, Log=FALSE, logfc.threshold = .1, minPct=0.1 )

expect_true( length( x@usedObj$deg.genes) == 102, info = paste("wrong gene number c++ wilcox", length( gene2) ) )
#x= logStatResult ( x, x@usedObj$sigGeneLists$Cpp[[x@usedObj$lastGroup]],method= 'wilcox', 'p.value')

ofile=  file.path( x@usedObj$sessionPath, 'AB_Stats_paritalLog.Rmd' )
expect_true( file.exists( ofile), label = ofile)


context('log figure - figure next')

png( file= file.path( x@usedObj$sessionPath, 'testScreenshot.png') ,width=800, height=800)
plot(1:10,10:1, main="Not reall a screenshot :-D")
grDevices::dev.off()

x= logFigure( x, png=file.path( x@usedObj$sessionPath, 'testScreenshot.png'), text="Not really a Screenshot, but a test file...") 


ofile=  file.path( x@usedObj$sessionPath, 'AC_Figure_paritalLog.Rmd' )
expect_true( file.exists( ofile), label = ofile)


## And the push a heatmap after that - this does not work in VR - hence thsi test!

context('log figure - heatmap last')

grDevices::png( file= file.path( x@usedObj$sessionPath, 'testHeatmap.png') ,width=800, height=800)
plot(10:1,1:10, main="Not reall a heatmap :-D")
grDevices::dev.off()

cellexalObj = logHeatmap(x, genes=x@usedObj$deg.genes, 
	png=file.path( x@usedObj$sessionPath, 'testHeatmap.png'), grouping = grouping )


files = c(
'AA_Start_paritalLog.Rmd',
'AB_Stats_paritalLog.Rmd',
'AC_Figure_paritalLog.Rmd',
'AD_Heatmap_paritalLog.Rmd'
)

for ( ofile in files ) {
	expect_true( file.exists( 
		file.path( x@usedObj$sessionPath, ofile ) ), label = ofile)
}

cellexalObj = renderReport( cellexalObj )

#session-log-for-session-2021-01-25-11-18-32.html
#session-log-for-session-logFigureTest.html
ofile=  file.path( x@outpath, 'session-log-for-session-logfiguretest.html' )
expect_true( file.exists( ofile), label = ofile)

ofile=  file.path( x@outpath, 'PortableLog_logFigureTest.zip' )
expect_true( file.exists( ofile), label = ofile)

check = list(
	'"logFigureTest/png/testScreenshot.png"' = 0
)

expect = list(
	'"logFigureTest/png/testScreenshot.png"' = 1
)

expect[[x@outpath]] = 0
check[[x@outpath]] = 0

check = checkFile(check,  file.path( x@outpath, 'session-log-for-session-logfiguretest.html' ))
expect_equal(check, expect, label="html file is OK") 

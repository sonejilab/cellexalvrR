context('heatmapOutfiles')

prefix = '.'

if ( file.exists( file.path(prefix, 'function.R'))){
	## the test functions that should not be maintained in differet test scripts...
	source( file.path(prefix, 'function.R')) 

}
#genes <- file.path(prefix, 'data/heatmap_0.txt')

#genes = utils::read.delim(genes)[,1]

#cellexalObj <- loadObject(file.path(prefix,'data','cellexalObjOK.RData') )

x = reset(cellexalObj)
x@outpath = file.path(prefix,'data','output','heatmapOutfiles' )


if ( file.exists(x@outpath)){
	unlink( x@outpath, recursive=TRUE )
}
dir.create(x@outpath)

x = sessionPath( x, 'heatmapOutfilesTest' )

grouping <- file.path(prefix, 'data/selection1.txt')

ofile= file.path(x@outpath,'heatmap_13-37-30.txt')
cellexalObj = make.cellexalvr.heatmap.list(x, grouping, 250, ofile,	"wilcox")

x= renderReport(x)

expect = c(  
	"PortableLog_heatmapOutfilesTest.zip", "cellexalObj.RData", 
	"heatmapOutfilesTest", "heatmap_13-37-30.txt", 
	"heatmap_13-37-30.txt.sqlite3", "libs", 
	"reference-keys.txt", "search_index.json")

for ( f in expect){
	expect_true( file.exists(file.path(x@outpath, f)), label = f )
}







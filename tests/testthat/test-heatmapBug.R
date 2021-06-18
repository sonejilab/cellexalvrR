context('heatmapBug')

prefix= './'
#prefix = 'tests/testthat'

path = file.path( prefix,'data', 'output', 'heatmapBug')
if ( file.exists( path)) {
	unlink(path, recursive=TRUE)
}
dir.create(path)

cellexalObj@outpath = path

cellexalObj= sessionPath( cellexalObj, 'test')
selection = file.path( prefix,'data', 'selection11.txt')

## output files from VR:
heatmap =  file.path(prefix,'data', "heatmap_07-36-01_2021-06-03-07-36-52.png")
expect_true( file.exists( heatmap ), label="VR heatmap exists")

expect_true( file.exists(selection), label=paste("selection file", selection) )

#cellexalObj = getDifferentials( cellexalObj, selection ) ## should be a 3 group selection.
dir.create( file.path( path, 'Heatmaps'))
cellexalObj = make.cellexalvr.heatmap.list(cellexalObj, cellidfile = selection, num.sig=250, outfile= file.path(path, 'Heatmaps' , 'heatmap_07-36-01.txt') )

files = c(  "AA_Start_test.html","AB_Stats_test.html", 
	"AC_OneGroupTime_test.html", "Heatmaps", "mainServer.sessionName", "selection11.txt",
  "selection11.txt.time", "selection11.txt.time.points", "test" )

expect_equal(list.files(path), files, label="All expected outfiles #1" )

files = c("heatmap_07-36-01.txt", "heatmap_07-36-01.txt.sqlite3" )
expect_equal( list.files(file.path(path, 'Heatmaps')), files , label="Heatmap VR files" )



## create a heatmap file that should show the timeline.
file.copy( heatmap, file.path(path, 'Heatmaps') )
png = file.path(path, 'Heatmaps', basename(heatmap)) 

selection2 = file.path( prefix,'data', 'selection10.txt')
cellexalObj = make.cellexalvr.heatmap.list(cellexalObj, cellidfile = selection2, num.sig=250, outfile= file.path(path, 'Heatmaps' , 'TestHeatmap2') )

cellexalObj = logHeatmap( cellexalObj, png=png, grouping=cellexalObj@usedObj$lastGroup  )

selectionFiles = lapply( cellexalObj@groupSelectedFrom, function(x){x@selectionFile})
for( f in selectionFiles){
	expect_true( file.exists(file.path(cellexalObj@outpath, f)), label=paste("selection file", f))
}
#cellexalObj@groupSelectedFrom

cellexalObj = renderReport( cellexalObj )

files = c( "cellexalObj.RData", "Heatmaps", "libs", 'png', "PortableLog_test.zip", 
	"reference-keys.txt", "search_index.json","selection10.txt", "selection11.txt", "selection11.txt.time" , 
	"selection11.txt.time.points", "session-log-for-session-test.html", "test")

expect_equal(sort(list.files(path)), sort(files), label="All expected outfiles #2" )


source( 'function.R' )

collect = list(

	"a href='./test/selection11.txt' download" = 0,
	"a href='./test/tables/Time.group.2.Linear.csv' download" = 0,
	"a href='./test/tables/User.group.3.Cpp.csv' download" = 0,
	"a href='./test/selection10.txt' download" = 0,
	'Time.group.2' = 0,
	'User.group.3' = 0
)

expect = list(

	"a href='./test/selection11.txt' download" = 1,
	"a href='./test/tables/Time.group.2.Linear.csv' download" = 1,
	"a href='./test/tables/User.group.3.Cpp.csv' download" = 1,
	"a href='./test/selection10.txt' download" = 1,
	'Time.group.2' = 26,
	'User.group.3' = 13
)

collect = checkFile( collect, file.path(path, "session-log-for-session-test.html") )

expect_equal( collect, expect, label="html outfile contents")
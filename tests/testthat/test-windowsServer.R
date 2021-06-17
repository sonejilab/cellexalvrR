context('server Simple SIMPLE on windows')

if ( .Platform$OS.type != 'windows' ) {
	skip('Test only meaningful under windows')
}
skip('not working')

prefix= './'
#prefix = 'tests/testthat'

path = file.path( prefix,'data', 'output','serverAsFunction')

if ( file.exists( path)){
	unlink( path, recursive=TRUE)
}

dir.create( path )
cellexalObj = reset(cellexalObj)
cellexalObj@outpath = normalizePath(path)
#oldwd = getwd()
#setwd( cellexalObj@outpath )


setTimeLimit(10,10)

fileConn<-file(file.path(cellexalObj@outpath,"mainServer.input.R" ))
writeLines(c("unlink('mainServer.pid')"
	), fileConn)
close(fileConn)


server ( file=file.path(cellexalObj@outpath,'mainServer'), debug=TRUE, asFunction=TRUE)


setTimeLimit(Inf,Inf)

files = c(  "mainServer.cellexalvrR.version", "mainServer.pid", "mainServer.sessionName" )

for ( f in files ){
	expect_true( file.exists( file.path( cellexalObj@outpath, f) ), label=f ) 
}

files = list.files(path)
sessionPath =  files[grep( "^\\d+_\\d+_\\d+_\\d+_\\d+_\\d+$",files )]

files = c( "_output.yaml", "1_Start_runRender.R", "AA_Start_paritalLog.Rmd", "png",
   "RsessionInfo.txt", "table.css", "tables")

expect_equal( files, list.files(file.path(path, sessionPath)), label ="All expected files in the session path")

#setwd( oldwd )
print("All is finished")

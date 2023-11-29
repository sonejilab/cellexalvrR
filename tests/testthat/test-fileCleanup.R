context('fileCleanup')

if ( ! expect_true( rmarkdown::pandoc_available() ,label= "pandoc is installed") ){
	skip ( "Pandoc needed - but missing here")
}
Sys.setenv(TEST_MODE = "TRUE")
prefix = '.'
opath = file.path(prefix, 'data','output', 'fileCleanup')

## now lets get a as simple session as possible:
if ( ! file.exists(file.path(prefix, 'data','output') )){
	dir.create( file.exists(file.path(prefix, 'data','output') ), recursive=TRUE)
}

cellexalObj = reset(cellexalObj)

cellexalObj@outpath = opath

cellexalObj = sessionPath( cellexalObj, NULL) ## the way cellexalVR does it

sessionA = cellexalObj@usedObj$sessionName

expect_true( file.exists(file.path(opath,sessionA)) , label="session path created")


cellexalObj = renderReport( cellexalObj )

expect_true( file.exists(file.path(opath,sessionA)) , label="session path exists after renderReport")

portableLog = file.path( opath,paste( sep="","PortableLog_",sessionA,".zip"))
expect_true( file.exists( portableLog ) , label="The portable log has been created")

Sys.sleep(1)

cellexalObj = sessionPath( cellexalObj, NULL)

expect_true( ! file.exists(file.path(opath,sessionA)) , label="old session path got removed")

cellexalObj = renderReport( cellexalObj )

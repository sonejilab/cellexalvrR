#' A main function in the log system - called after each log has been produced.
#' @name renderFile
#' @docType methods
#' @description render only one html section, not the whole session log
#' @param x  the cellexalvrR object
#' @param id the id of the report file to render ('x@usedObj$sessionRmdFiles[id]')
#' @param type the type of log saved (default '')
#' @title render only one html section, not the whole session log
#' @export 
setGeneric('renderFile', ## Name
	function ( x, id, type='' ) { ## Argumente der generischen Funktion
		standardGeneric('renderFile') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)


#' @rdname renderFile
setMethod('renderFile', signature = c ('cellexalvrR'),
	definition = function ( x, id, type='' ) {

if ( is.null( x@usedObj$sessionPath )) {
		x = sessionPath(x) #function definition in file 'sessionPath.R'
}
sessionPath = normalizePath(x@usedObj$sessionPath)
if ( length(list.files(sessionPath, pattern =paste(sep="","*",type,"*.html" ))) > 0 ){
	if ( type=='Start') {
		return (x) ## THIS MUST NOT BE THERE TWICE
	}
	if ( type =='End') {
		return (x) ## THIS MUST NOT BE THERE TWICE
	}
}
	fname = x@usedObj$sessionRmdFiles[id]
	if ( ! file.exists(fname) ){
		stop( paste( "fname for the sessionRmdFiles id",id,",", fname,"does not exists on the file system"))
	}
	
	fileConn<-file(file.path(sessionPath, '_output.yaml') )
	AA = as.vector( sapply(LETTERS, function(x) paste0(x, LETTERS)))

	writeLines(c(
		paste('book_filename:', paste(AA[id], type, x@usedObj$sessionName, sep="_" )),
		'output_dir: ../',	
		'output:',
		'  html_document:',
		'    css: "table.css"',
		'delete_merged_file: true' )
		, fileConn 
	) 
    close(fileConn)

    if ( ! file.exists( file.path(sessionPath,"table.css" )) ) {
		fileConn<-file(file.path(sessionPath,"table.css" ) )
		writeLines(c(
			'table, th, td {',
			'  border: 1px solid black;',
			'  border-collapse: collapse;',
			'  padding: 15px;',
			'}')
			, fileConn 
		)
		close(fileConn)
	}
    message( paste('bookdown::render_book log id', id, "/", AA[id] ) )
	## and now a bloody hack:
	oldwd = getwd()
	setwd( x@usedObj$sessionPath )
	if ( id != 1 ) {
		files = as.character(unlist(lapply( x@usedObj$sessionRmdFiles[c(1,id)], basename)))
	}else {
		files =  basename(x@usedObj$sessionRmdFiles[1])
	}
	
	cmd =c( paste( sep="","setwd( ", file2Script( sessionPath ), " )\n"),
	 paste( sep="","markdown::markdownToHTML(file=",file2Script(fname),
		",  output='",file.path('..',
		paste(sep=".",paste(AA[id], type, x@usedObj$sessionName, sep='_' ),"html")),"',  stylesheet= 'table.css' )") )

	script = paste( sep="_", id,type,"runRender.R")
	if ( file.exists( script)) {
		unlink( script )
	}
	cat( paste(sep="\n",cmd), file=script, append=FALSE )
	
	tryCatch( 
	{
	system( paste(Rscript.exe(), script ), intern=TRUE)
	}, 
	error=function(er) { 
		print ( paste(sep="\n","renderFile: Pandoc call failed:" , er) )
	} 
	)
	message( paste('bookdown::render_book log id', id, 'finished') )

	setwd( oldwd )
	x
} )


#' @name getUniqueFile
#' @aliases getUniqueFile,cellexalvrR-method
#' @rdname getUniqueFile
#' @docType methods
#' @description store the Rmd contens produced in a logXYZ function into a file and register that one.
#' @param file_path the path for the new file
#' @param file_name the name of the new file (including the extension)
#' @param file_ext the extension of the new file
#' @title preceed the file_core with AA_ to XX_ for all file_path with the  file_ext extension
#' @export 
setGeneric('getUniqueFile', ## Name
	function ( file_path, file_name, file_ext ) { ## Argumente der generischen Funktion
		standardGeneric('getUniqueFile') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)


#' @rdname getUniqueFile
setMethod('getUniqueFile', signature = c ('character'),
	definition = function ( file_path, file_name, file_ext ) {
		id = length( list.files ( file_path, pattern=paste( sep="",'*', file_ext,'$'), full.names =TRUE ) ) 
		AA = as.vector( sapply(LETTERS, function(x) paste0(x, LETTERS)))
		fname = paste( sep="_", AA[id+1], file_name )
		return ( file.path( file_path, fname ))
	}
)


#' @name storeLogContents
#' @aliases storeLogContents,cellexalvrR-method
#' @rdname storeLogContents
#' @docType methods
#' @description store the Rmd contens produced in a logXYZ function into a file and register that one.
#' @param x the CellexalvrR object
#' @param content the text content of the Rmd file
#' @param type the type of log saved (default '')
#' @title write a (Rmd) string and add that to the log 
#' @export 
setGeneric('storeLogContents', ## Name
	function ( x, content, type='' ) { ## Argumente der generischen Funktion
		standardGeneric('storeLogContents') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)


#' @rdname storeLogContents
setMethod('storeLogContents', signature = c ('cellexalvrR'),
	definition = function ( x, content, type='' ) {
		
	if ( is.null( x@usedObj$sessionPath )){
		x = sessionPath(x) #function definition in file 'sessionPath.R'
	}
	sessionPath = normalizePath(x@usedObj$sessionPath)
	fname = NULL
	if ( type =="Start"){
		fname = paste( sep="_", "AA", type, "paritalLog.Rmd" )
		fname = file.path( sessionPath, fname )
	}else {
		fname = getUniqueFile( sessionPath, paste( sep="_", type, "paritalLog.Rmd"), 'Rmd' )
	}
	x@usedObj$sessionRmdFiles =
	 	list.files( sessionPath , pattern='*Rmd$', full.names =TRUE )

	file.create( fname )

	x@usedObj$sessionRmdFiles = c(x@usedObj$sessionRmdFiles, fname )
	
	fileConn<-file( fname )
	writeLines( content, fileConn )
    close(fileConn)
    x
} )

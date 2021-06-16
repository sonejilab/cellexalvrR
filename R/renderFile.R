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
	
	if ( type !="Start"){
		id = length( list.files ( x@outpath, pattern='*html$', full.names =TRUE ) ) 

		if ( length(x@usedObj$sessionRmdFiles ) != id ) {
			x@usedObj$sessionRmdFiles =
		 	list.files( sessionPath , pattern='*Rmd$', full.names =TRUE )
		}
		id = id +1
	}else {
		id = 1
	}

	AA = as.vector( sapply(LETTERS, function(x) paste0(x, LETTERS)))
	fname = paste( sep="_", AA[id], type, "paritalLog.Rmd" )
	fname = file.path( sessionPath, fname )

	if ( file.exists( fname )) {
		## damn - somewhere the cellexal object was not saved - better read in all Rmd files now
		x@usedObj$sessionRmdFiles = list.files(sessionPath, full.names = TRUE, pattern='*.Rmd')
		id = length(x@usedObj$sessionRmdFiles) +1
		AA = as.vector( sapply(LETTERS, function(x) paste0(x, LETTERS)))
		fname = paste( sep="_", AA[id], type, "paritalLog.Rmd" )
		fname = file.path( sessionPath, fname )
	}
	if ( file.exists(fname) ){
		stop(paste( "The outfile already existed!" , fname))
	}

	file.create( fname )

	x@usedObj$sessionRmdFiles = c(x@usedObj$sessionRmdFiles, fname )
	
	fileConn<-file( fname )
	writeLines( content, fileConn )
    close(fileConn)
    x
} )

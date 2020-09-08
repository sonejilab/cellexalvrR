#' @name renderFile
#' @aliases renderFile,cellexalvrR-method
#' @rdname renderFile-methods
#' @docType methods
#' @description render only one html section, not the whole session log
#' @param x  the cellexalvrR object
#' @param id the id of the report file to render ('x@usedObj$sessionRmdFiles[id]')
#' @param type the type of log saved (default '')
#' @title description of function renderFile
#' @export 
setGeneric('renderFile', ## Name
	function ( x, id, type='' ) { ## Argumente der generischen Funktion
		standardGeneric('renderFile') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('renderFile', signature = c ('cellexalvrR'),
	definition = function ( x, id, type='' ) {

if ( is.null( x@usedObj$sessionPath )){
		x = sessionPath(x) #function definition in file 'sessionPath.R'
	}
		sessionPath = normalizePath(x@usedObj$sessionPath)
	fname = x@usedObj$sessionRmdFiles[id]
	if ( ! file.exists(fname) ){
		stop( paste( "fname for the sessionRmdFiles id",id,",", fname,"does not exists on the file system"))
	}
	
	fileConn<-file(file.path(sessionPath, '_bookdown.yml') )
	writeLines(c(
		paste('book_filename:', paste(id, type, x@usedObj$sessionName, sep="_" )),
		'output_dir: ../',			
		'delete_merged_file: true' )
		, fileConn 
	) 
    close(fileConn)
    message( paste('bookdown::render_book log id', id) )
	## and now a bloody hack:
	oldwd = getwd()
	setwd( x@usedObj$sessionPath )
	if ( id != 1 ) {
		files = as.character(unlist(lapply( x@usedObj$sessionRmdFiles[c(1,id)], basename)))
	}else {
		files =  basename(x@usedObj$sessionRmdFiles[1])
	}
	
	cmd =c( paste( sep="","setwd( ", file2Script( sessionPath ), " )\n"), paste( sep="","rmarkdown::render(input=",file2Script(fname),
		", output_format= 'html_document', output_file='",
		paste(id, type, x@usedObj$sessionName, sep='_' ),"', output_dir='../')") )

	script = paste( sep="_", id,"runRender.R")
	if ( file.exists( script)) {
		unlink( script )
	}
	cat( paste(sep="\n",cmd), file=script, append=FALSE )
	
	system( paste(Rscript.exe(), script ), intern=TRUE)
	message( paste('bookdown::render_book log id', id, 'finished') )

	setwd( oldwd )
	x
} )


#' @name storeLogContents
#' @aliases storeLogContents,cellexalvrR-method
#' @rdname storeLogContents-methods
#' @docType methods
#' @description store the Rmd contens produced in a logXYZ function into a file and register that one.
#' @param x the CellexalvrR object
#' @param content the text content of the Rmd file
#' @param type the type of log saved (default '')
#' @title description of function storeLogContents
#' @export 
setGeneric('storeLogContents', ## Name
	function ( x, content, type='' ) { ## Argumente der generischen Funktion
		standardGeneric('storeLogContents') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('storeLogContents', signature = c ('cellexalvrR'),
	definition = function ( x, content, type='' ) {

	if ( is.null( x@usedObj$sessionPath )){
		x = sessionPath(x) #function definition in file 'sessionPath.R'
	}
	sessionPath = normalizePath(x@usedObj$sessionPath)
	id = length(x@usedObj$sessionRmdFiles) +1
	fname = paste( sep="_", id, type, "paritalLog.Rmd" )
	fname = file.path( sessionPath, fname )

	if ( file.exists( fname )) {
		## damn - somewhere the cellexal object was not saved - better read in all Rmd files now
		x@usedObj$sessionRmdFiles = list.files(sessionPath, full.names = TRUE, pattern='*.Rmd')
		id = length(x@usedObj$sessionRmdFiles) +1
		fname = paste( sep="_", id, type, "paritalLog.Rmd" )
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

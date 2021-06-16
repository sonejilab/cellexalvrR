#' VR method that creates the session specific settings for each VR session.
#' @name sessionPath
#' @docType methods
#' @description Use the session ID and object outpath to create a session path for the reports
#' @param cellexalObj the cellexal object
#' @param sessionName the session ID default=NULL
#' @title create/restore a new/unfinished session
#' @export
setGeneric('sessionPath', ## Name
		function (cellexalObj, sessionName=NULL ) { 
			standardGeneric('sessionPath')
		}
)


#' @rdname sessionPath
setMethod('sessionPath', signature = c ('cellexalvrR'), 
	definition = function (cellexalObj, sessionName=NULL ) {
			
	if ( ! is.null(cellexalObj@usedObj$sessionName) ){
		if ( ! is.null(sessionName)){
			if ( interactive()) {
			warning(paste(
				"new session name", sessionName , 
				"is ignored in favor of the existsing session name",
				" - use renderReport() to clear the old session") )
			}
		}
		return (invisible (cellexalObj));
	}

	## there seams to be a problem in the interplay between VR and R
	## sometimes I get two cellexal outfolders in one session
	## That does not make sense and therefore this here needs an upgrade.
	if ( ! file.exists( cellexalObj@outpath) ) {
		## oops that should have been fixed before - but who cares?
		dir.create( cellexalObj@outpath, recursive=TRUE )
	}

	removeOld = TRUE
	if ( file.exists( file.path( cellexalObj@outpath, 'mainServer.pid'))) {
		#warning('debug: mainServer.pid exists')
		pid = scan( file.path( cellexalObj@outpath, 'mainServer.pid'), quiet = TRUE )
		#warning (paste("debug: I got the pid", pid))
		masterPID = ps::ps_handle( pid = as.integer(pid) )
		if ( ps::ps_is_running( masterPID ) ) {
			#warning ("debug: and the process is active")
			if ( file.exists(file.path(cellexalObj@outpath,'mainServer.sessionName') )){
				sessionName = 
					scan( file.path(cellexalObj@outpath,'mainServer.sessionName'), what=character(), quiet = TRUE )
			
				#warning(paste(sep="",
				#	"debug:  server is running and using session '",
				#	sessionName, 
				#	"' I will also add to that session!" ) 
				#)
				removeOld = FALSE
			}
		}else {
			unlink( file.path( cellexalObj@outpath, 'mainServer.pid') )
		}

	}

	if ( ! is.null(sessionName) ){
		cellexalObj@usedObj$sessionName = sessionName
		cellexalObj@usedObj$sessionRmdFiles = NULL
		cellexalObj@usedObj$sessionPath = NULL
		cellexalObj@usedObj$sessionCounter = NULL
		cat( cellexalObj@usedObj$sessionName , file = file.path(cellexalObj@outpath,'mainServer.sessionName') )
	}
	else if ( is.null(cellexalObj@usedObj$sessionName )) {
		
		if ( is.null( cellexalObj@usedObj$sessionName ) ){
			cellexalObj@usedObj$sessionName = 
				filename( as.character(Sys.time())) #function definition in file 'filename.R'
			cat( cellexalObj@usedObj$sessionName , file = file.path(cellexalObj@outpath,'mainServer.sessionName') )
		}
		cellexalObj@usedObj$sessionRmdFiles = NULL
		cellexalObj@usedObj$sessionPath = NULL
		cellexalObj@usedObj$sessionCounter = NULL
	}
	cat( cellexalObj@usedObj$sessionName , file = file.path(cellexalObj@outpath,'mainServer.sessionName') )

	## now the session name is fixed and we can think about the internals

	if ( is.null(cellexalObj@usedObj$sessionPath) ) {
		## init the session objects
		## add a simple session log start file
		cellexalObj@usedObj$sessionPath = file.path(cellexalObj@outpath, cellexalObj@usedObj$sessionName)
		
		if ( removeOld ){
			## I need to clear out all old session report Rmd and html files
			t = do.call(file.remove, list(list.files( cellexalObj@usedObj$sessionPath, full.names = TRUE, pattern="*.Rmd" )))
			htmls = list.files( cellexalObj@outpath , full.names = TRUE, pattern=".*.html" )
			#bad = htmls[ grep( 'session-log-for-session',  htmls,  invert=TRUE )]
			bad = htmls
			if ( length(bad) > 0 ) {
				t = do.call(file.remove, list(bad) )
			}
			#selectionFiles = list.files( file.path(cellexalObj@usedObj$sessionPath, '..'), full.names = TRUE, pattern="selection.*" )
			#t = do.call(file.remove, list(selectionFiles) )

			## now lets get rid of all old folders, too.
			dirs = list.dirs(cellexalObj@outpath)
			dirs = dirs[ grep( "\\d+_\\d+_\\d+_\\d+_\\d+_\\d+$", dirs) ]
			if ( length(dirs) > 0 ){
				rem = function(x) { unlink(x, recursive=TRUE)}
				t = do.call(rem, list(dirs) )
			}
		}

		if (! dir.exists(cellexalObj@usedObj$sessionPath) )  {
			message( paste("I try to create the session path here! - ", cellexalObj@usedObj$sessionPath ))
			dir.create( cellexalObj@usedObj$sessionPath, recursive = TRUE)
			dir.create( file.path( cellexalObj@usedObj$sessionPath, 'png'), recursive = TRUE)
			dir.create( file.path( cellexalObj@usedObj$sessionPath, 'tables'), recursive = TRUE)
		}
		if (! dir.exists(file.path(cellexalObj@usedObj$sessionPath, 'png') ) )  {
			dir.create( file.path( cellexalObj@usedObj$sessionPath, 'png'), recursive = TRUE)
			dir.create( file.path( cellexalObj@usedObj$sessionPath, 'tables'), recursive = TRUE)
		}

		content = c(
			paste( sep="\n", '---', paste(sep=" ",'title:',cellexalObj@usedObj$sessionName ), "---"),
			paste( "# Session Log for Session", cellexalObj@usedObj$sessionName ),
			paste( "Analysis of data: ", basename(cellexalObj@outpath) ),
			"",
			paste( "Started on", format(Sys.time(), "%a %b %d %X %Y") ),
			""
		)
		cellexalObj@usedObj$sessionPath = 
			normalizePath( cellexalObj@usedObj$sessionPath )

		cellexalObj = storeLogContents( cellexalObj, content, type='Start')
		id = length(cellexalObj@usedObj$sessionRmdFiles)
		cellexalObj = renderFile( cellexalObj, id, type='Start' )
		
		savePart(cellexalObj,part = 'usedObj' ) #function definition in file 'integrateParts.R'
	}
	invisible(cellexalObj)
})



#' @rdname sessionPath
setMethod('sessionPath', signature = c ('character'),
		definition = function (cellexalObj, sessionName=NULL) {
			cellexalObj <- loadObject(cellexalObj) #function definition in file 'lockedSave.R'
			sessionPath(cellexalObj, sessionName ) #function definition in file 'sessionPath.R'
		}
)

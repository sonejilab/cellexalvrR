
#' This server function is the main speed up for big data files.
#'
#' Instead of loading and saving all files for each function call in the VR process
#' we now can have one R process that sequencially works on all requests.
#'
#' The server will look for a file called <file>.input.R and process all commands in the file 
#' using source(fname) so please be careful there!
#'
#' While the file <file>.input.lock is existing the input.R file is processed by the R and should not be touched.
#' 
#' @name server
#' @docType methods
#' @description starts a server looking for a sourcable file in paste(file, 'input.R', sep='.')
#' The function sets a paste(file, 'input.lock', sep='.') lock file on processed scripts.
#' To shut down the server process you can either write a q('no') into the script file or remove the pid file.
#' @param file the file core name to create input.R input.log and pid files.
#' @param sleepT sleep time in seconds between checks for the input.R file (default 1)
#' @param debug create the server output file? default FALSE
#' @param masterPID if this pid is not active any more stop the server (default NULL)
#' @param asFunction do not shut down R when exiting (default FALSE)
#' @keywords server
#' @title start a server function periodicly sourcing in a file.
#' @export server
#if ( ! isGeneric('renew') ){
setGeneric('server', ## Name
			function ( file, sleepT=1, debug=FALSE, masterPID = NULL, asFunction =FALSE ) { 
				standardGeneric('server') 
			}
)
#}



#' @rdname server
setMethod('server', signature = c ('character'),
		definition =  function(file, sleepT=1, debug=FALSE, masterPID = NULL, asFunction =FALSE ){
	lockfile   = paste( file, 'input.lock', sep=".") 
	scriptfile = paste( file, 'input.R', sep="." )
	pidfile    = paste( file, 'pid', sep='.')

	workSource = paste( file, 'input.working', sep="." )

	t = lapply( c( lockfile, pidfile) ,
	  function(file) {
	    if(file.exists(file)) { unlink(file) } 
	  }
	)
	if ( ! asFunction ) {
	  if(file.exists(scriptfile)) { unlink(scriptfile) }
	}

	cat( Sys.getpid() , file = pidfile )
	
	outFile = file(paste( file,  Sys.getpid(), 'output', sep=".") )
	
	if (! is.null(masterPID)) {
		masterPID = ps::ps_handle( pid = as.integer(masterPID) )
	}

	if ( debug ){
		# package version needs to be exported
		pv_file    = paste( file, 'cellexalvrR.version', sep='.')
		file.create(pv_file)
		cat( as.character(utils::packageVersion("cellexalvrR")), file= pv_file, append=F)
		## redirect append output to output file
		## Error is captured by the VR application and this is important to leave it like that.
		if ( ! asFunction ) { sink(outFile) }
	}


	cellexalObj@outpath =  dirname(file)
	cellexalObj@usedObj$sessionName = NULL
	cellexalObj = sessionPath(cellexalObj)

	fileConn<-file(file.path(cellexalObj@usedObj$sessionPath,"RsessionInfo.txt" ))
	writeLines(utils::capture.output(utils::sessionInfo()), fileConn)
	close(fileConn)

	message ( paste( "server is starting - reading from file:\n", scriptfile))
	message ( paste( "server debug mode:", debug))
	if ( !is.null(masterPID) ) {
		message ( paste( "server is checking the process",masterPID,"and shuts down if it is shutted down"))
	}

	## we check for additional screenshots in folder
	path = file.path(dirname(file), 'Screenshots')
	oldFiles = newScreenshots( c(), path =path )
	oldFilters = newFilters( c(), path= cellexalObj@outpath)

  	while(TRUE){
  		newFiles = newScreenshots(oldFiles, path=path)
  		if ( length(newFiles) > 0 ){

			for (n in newFiles) {
				message( paste( 'logFigure will log the file',file.path(path, n) ))
				cellexalObj = logFigure( cellexalObj,png= file.path(path, n), text='New VR screenshot.' )
			}
			oldFiles = c( oldFiles, newFiles)
		}
		newFilters = newFilters( oldFilters, path=cellexalObj@outpath )
		if ( length(newFilters) > 0 ){
			for (n in newFilters) {
				this = file.path( cellexalObj@usedObj$sessionPath, n )
				n = file.path(cellexalObj@outpath, n)
				message( paste( 'logFigure will log the file',n ))
				file.copy( n, this)
				filter= paste( collapse=" ", scan( this, what=character()))
				content = paste("\n## New Filter\n\nA new filter has been created in VR:\n\n```", filter,"```\n\n")
				cellexalObj = storeLogContents( cellexalObj, content, type="VRfilter" )
				id = length(cellexalObj@usedObj$sessionRmdFiles)
				cellexalObj = renderFile( cellexalObj, id, type='VRfilter' )
			}
			oldFilters = c( oldFilters, newFilters)
		}
		if ( ! file.exists(pidfile ) ) {
			break
		}
     	if ( file.exists( scriptfile ) ) {
            while ( file.exists( lockfile ) ) {
                Sys.sleep( sleepT )
        	}
			file.create(lockfile)
		
			cmd = readLines( scriptfile)
			cmd = stringr::str_replace_all( paste( collapse=" " ,cmd), '\\s+', ' ')
       		cat ( c("", cmd,""), file= outFile, sep="\n\r", append=TRUE )
        	message( paste("VR cmd:", cmd) ) ## will go into the R_log.txt
        	try ( {source( scriptfile, local=FALSE ) } )
        	file.remove(scriptfile)
        	file.remove(lockfile)
       	}
        if ( ! is.null( masterPID ) ){
        	if ( ! ps::ps_is_running( masterPID )) {
	        	message( paste("is the master",masterPID, "became inactive!") )
        		unlink( pidfile ) ## shutdown in the next cycle.
        	}
        }
        Sys.sleep( sleepT )   
	}
	message( "Server pid file lost - closing down" );
	if ( exists('cellexalObj') ) {
		if ( ! is.null(cellexalObj@usedObj$sessionName ) ) {
			message( "closing session" );
			cellexalObj = renderReport( cellexalObj )
			if ( debug ) {
				cat("cellexalObj = renderReport( cellexalObj )")
			}
		}
		message( "saving the main object" );
		#lockedSave( cellexalObj ) ##the renderReport does that
	}
	t = lapply( c( lockfile, scriptfile) , 
	function(file) { 
		if(file.exists(file)) { 
		unlink(file)} 
	})
	close(outFile)
	if ( debug ) {
		sink()
	}
	if ( ! asFunction ) {
		q('no')
	}
	
}
)




##' @name newScreenshots
##' @aliases newScreenshots,character-method
##' @rdname newScreenshots
##' @docType methods
##' @description Check the folder '../Screenshots' for files
##' @param oldFiles a vector of old files.
##' @param path the path to look for file (default '../Screenshots' )
##' @keywords server
##' @title internal function!
newScreenshots <- function(oldFiles, path ='../Screenshots/' ) {
		files = list.files( path )
		OK = NULL
		for ( x in files ){
			if ( all ( sapply( oldFiles, function(old) { ! old==x } )) ){
				OK = c(OK, x)
			}
		}
		OK
	}


##' @name newFilters
##' @aliases newFilters,character-method
##' @rdname newFilters
##' @docType methods
##' @description Check the folder '../' for .fil files
##' @param oldFiles a vector of old files.
##' @param path the path to look for file (default '../Screenshots' )
##' @keywords server
##' @title internal function!
newFilters <- function(oldFiles, path ='../' ) {
		files = list.files( path, pattern = "*.fil$" )
		OK = NULL
		for ( x in files ){
			if ( all ( sapply( oldFiles, function(old) { ! old==x } )) ){
				OK = c(OK, x)
			}
		}
		OK
	}
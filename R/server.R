#' This server function is the main speed up for big data files.
#'
#' Instead of loading and saving all files for each function call in the VR process
#' we now can have one R process that sequencially works on all requests.
#'
#' @name server
#' @aliases server,character-method
#' @rdname server-methods
#' @docType methods
#' @description starts a server looking for a sourcable file in paste(file, 'input.R', sep='.')
#' The function sets a paste(file, 'input.lock', sep='.') lock file on processed scripts.
#' To shut down the server process you can either write a q('no') into the script file or remove the pid file.
#' @param file the file core name to create input.R input.log and pid files.
#' @param sleepT sleep time in seconds between checks for the input.R file
#' @param debug create the server output file? default FALSE
#' @param masterPID if this pid is not active any more stop the server (default NULL)
#' @keywords server
#' @title start a server function periodicly sourcing in a file.
#' @export server

if ( ! isGeneric('server') ){setGeneric('server', ## Name
			function ( file, sleepT=1, debug=FALSE, masterPID = NULL ) { 
				standardGeneric('server') 
			}
	) }

setMethod('server', signature = c ('character'),
		definition =  function(file, sleepT=1, debug=FALSE, masterPID = NULL){
	lockfile   = paste( file, 'input.lock', sep=".") 
	scriptfile = paste( file, 'input.R', sep="." )
	pidfile    = paste( file, 'pid', sep='.')

	t = lapply( c( lockfile, scriptfile, pidfile) , function(file) { if(file.exists(file)) { unlink(file)} })

	cat( Sys.getpid() , file = pidfile )
	
	outFile = file(paste( file,  Sys.getpid(), 'output', sep=".") )

	if (! is.null(masterPID)) {
		masterPID = ps::ps_handle( pid = as.integer(masterPID) )
	}
	if ( debug ){
		# package version needs to be exported
		pv_file    = paste( file, 'cellexalvrR.version', sep='.')
		file.create(pv_file)
		cat( as.character(packageVersion("cellexalvrR")), file= pv_file, append=F)
		## redirect appendll output to output file
		sink(outFile)
	}
	message ( paste( "server is starting - reading from file:\n", scriptfile))
	message ( paste( "server debug mode:", debug))
	if ( !is.null(masterPID) ) {
		message ( paste( "server is checking the process",masterPID,"and shuts down if it is shutted down"))
	}
  	while(TRUE){
		if ( ! file.exists(pidfile ) ) {
			break
		}
        if ( file.exists( scriptfile ) ) {
                while ( file.exists( lockfile ) ) {
                        Sys.sleep( sleepT )
                }
				file.create(lockfile)
				if ( debug ) {
               	 cat ( readLines( scriptfile), file= outFile, sep="\n\r", append=TRUE )
            	}
                try ( {source( scriptfile ) } )
                file.remove( scriptfile )
				file.remove(lockfile)
        }
        if ( ! is.null( masterPID ) ){
        	#
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
		}
		message( "saving the main object" );
		#lockedSave( cellexalObj ) ##the renderReport does that
	}
	t = lapply( c( lockfile, scriptfile) , function(file) { if(file.exists(file)) { unlink(file)} })
	if ( debug ) {
		sink()
		close(outFile)
	}
	q('no')
}
)


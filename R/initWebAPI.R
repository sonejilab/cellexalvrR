#' Create a R plumber based server for this cellexalvrR object.
#' The abilites of this server are defined in the inst/extdata/webapi.R script.
#' @name initWebAPI
#' @docType methods
#' @param cellexalObj, cellexalvr object
#' @param port the port for this service default to 8000
#' @title start the web API as plumber server.
#' @export 
#if ( ! isGeneric('initWebAPI') ){
setGeneric('initWebAPI', ## Name
	function (cellexalObj, port=8000) { 
		standardGeneric('initWebAPI') 
	}
)
#}


#' @rdname initWebAPI
setMethod('initWebAPI', signature = c ('cellexalvrR'),
	definition = function (cellexalObj, port=8000) {
		target = file.path ( cellexalObj@outpath, "webapi.R" )
		if ( file.exists(target) ) { 
			unlink( target)
		}
		file.copy( system.file("extdata", "webapi.R",package= "cellexalvrR"), target )
		cellexalObj = sessionPath(cellexalObj)
		fileConn<-file(file.path(cellexalObj@usedObj$sessionPath,"RsessionInfo.txt" ))
		writeLines(utils::capture.output(utils::sessionInfo()), fileConn)
		close(fileConn)
		obj = plumber::pr( target )
		plumber::pr_run(obj, port=port)
	}
)

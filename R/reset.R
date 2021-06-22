#' Remove all modifications produced through VR run.
#' 
#' @name reset
#' @docType methods
#' @description resets the cellexalvrR object for a new VR session
#' @param x the cellexalObj you want to reset
#' @title remove all modifications added to this cellexalvrR object
#' @export 
setGeneric('reset', ## Name
	function ( x ) {
		standardGeneric('reset')
	}
) 


#' @rdname reset
setMethod('reset', signature = c ('cellexalvrR'), definition = function ( x ) {
	
	x@usedObj$sessionPath = NULL
	x@usedObj$sessionRmdFiles = NULL
	x@usedObj$sessionName = NULL
	x@userGroups=data.frame()
	x@usedObj$lastGroup = NULL
	x@usedObj$SelectionFiles = list()
	x@groupSelectedFrom = list()
	x@usedObj$linearSelections = list()
	x@usedObj$sigGeneLists = list()
	#x@outpath= getwd()
	if ( file.exists( file.path(x@outpath, 'mainServer.sessionName')) ){
		unlink( file.path(x@outpath, 'mainServer.sessionName') )
	}
	return (x)
})
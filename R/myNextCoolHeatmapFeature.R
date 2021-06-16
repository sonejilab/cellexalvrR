#' This function is linked to a button on the VR heatmaps.
#' In the default cellexalvrR version this function is a blank to allow non VR Bioinformaticians
#' to start a function they miss and want to implement themselves.
#' 
#' Unimplemented in VR. Future stuff.
#' @name myNextCoolHeatmapFeature
#' @docType methods
#' @description Unused function for future implementations.
#' @param x the cellexalvrR object
#' @param selection the selection file linked to the heatmap
#' @param outfile the outfile the VR process is waiting for
#' @title empty function to be changed by any user (future stuff)
#' @export 
#if ( ! isGeneric('renew') ){
setGeneric('myNextCoolHeatmapFeature', ## Name
	function (x, selection, outfile) { 
		standardGeneric('myNextCoolHeatmapFeature')
	}
)
#}


#' @rdname myNextCoolHeatmapFeature
setMethod('myNextCoolHeatmapFeature', signature = c ('cellexalvrR', 'character', 'character'),
	definition = function (x, selection, outfile) {

	# please implement you new functionality here:
	cellexalObj = loadObject(x)
	x = userGrouping( x, selection )

	# after you finished please create the outfile to message to the VR process that this function has finished
	fileConn<-file( outfile )
	writeLines( "Some text" , fileConn)
	close(fileConn)

	invisible(cellexalObj)
} )
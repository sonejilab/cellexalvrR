#' Copy all drc models from one cellexalObj to another - even if the
#' order and/or the size of the objects is not the same.
#' @name GrabDRCs
#' @docType methods
#' @description make 3D graphs from consecutive analysies available in the main analysis VR session
#' @param x the cellexalvrR object
#' @param other the other cellexalvrR object
#' @param prefix a prefix for the 3D graph name (shown in VR)
#' @title copy 3D graphs from objects of the same analysis
#' @export 
setGeneric('GrabDRCs', ## Name
	function ( x, other, prefix = NULL ) {
		standardGeneric('GrabDRCs')
	}
)


#' @rdname GrabDRCs
setMethod('GrabDRCs', signature = c ('cellexalvrR', 'cellexalvrR'),
	definition = function ( x, other, prefix = NULL) {
	if ( is.null(prefix) ) {
		stop("Sorry but I need a prefix to add the 3D graphs from another cellexalvrR object.")
	}
	m = match(colnames(other@data), colnames(x@data) )
	bad = which(is.na(m)) 
	if ( length(bad) > 0 ){
		m = m[-bad]
		warning(paste(length(bad), "cells from the other object are unknown here!") )
	}
	if ( length(m) == 0 ) {
		stop("None of the other cells are known herer - STOP")
	}

	for ( n in names( other@drc )) {
		new_name = paste(sep="_", prefix, n)
		if ( ! is.null(x@drc[[new_name]])) {
			stop(paste( "The drc with the name", new_name, "is already exisiting in the object") )
		}
		if ( is.null(rownames(other@drc[[n]]))){
			rownames(other@drc[[n]]) = colnames(other@data)
		}
		x@drc[[new_name]] = other@drc[[n]][order(m),]
	}

	invisible( check(x) )
} )
#' logText will create a section in the log document
#' The document or section will only contain the text you give me.
#' @name logText
#' @rdname logText
#' @docType methods
#' @description preload the object before creating one Heatmap session report page
#' @param cellexalObj the cellexalvrR object
#' @param text the that the log should contain.
#' @title add one section with only text
#' @export 
setGeneric('logText', ## Name
	function ( cellexalObj, text=NULL ) {
		standardGeneric('logText')
	}
	)


#' @rdname logText
setMethod('logText', signature = c ('cellexalvrR'),
	definition = function ( cellexalObj, text=NULL ) {
	## here I need to create a page of the final log
	if ( is.null(text)){
		return ( cellexalObj )
	}
	cellexalObj = storeLogContents( cellexalObj, content, type="TextOnly")
	id = length(cellexalObj@usedObj$sessionRmdFiles)
	cellexalObj = renderFile( cellexalObj, id, type="TextOnly" )
	
	return (cellexalObj	)
})
	
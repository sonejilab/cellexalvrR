#' A log helper function. It does remove the need to store all log Rmd files in the object.
#' @name sessionCounter
#' @description returns the counter for this session and this group name
#' @param x the cellexalvrR object
#' @param gName the group name
#' @title an internal function to get the ID for the next log file
#' @export 
setGeneric('sessionCounter', ## Name
	function ( x, gName ) { 
		standardGeneric('sessionCounter')
	}
)


#' @rdname sessionCounter
setMethod('sessionCounter', signature = c ('cellexalvrR'),
	definition = function ( x, gName ) {
		ret = NA
		if (  is.na( match(gName, names(x@usedObj$sessionCounter)) ) == FALSE ){
			ret = x@usedObj$sessionCounter[[gName]]
		}
		ret
} )

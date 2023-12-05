#' @name getLinearSelection
#' @docType methods
#' @description Accessor function to get a cellexalLinear object from a cellexalvrR object by name.
#' @param cellexalObj the object to get the data from
#' @param name the name for this linearSelection
#' @title get an existing linearSelection or throw an error
#' @examples \dontrun{ 
#'	getLinearSelection( cellexalObj, 'Time.group.2') 
#' }
#' @export
#if ( ! isGeneric('getLinearSelection') ){
setGeneric('getLinearSelection', ## Name
	function (  cellexalObj, name ) { 
		standardGeneric('getLinearSelection')
	}
)
#}

#' @rdname getLinearSelection
setMethod('getLinearSelection', signature = c ('cellexalvrR', 'list'),

	definition = function ( cellexalObj, name ) {

		doIt = function (d) { 
			if (class(d) == "cellexalGrouping" ){
				d@linarObj
			}else{
				d
			}
		}
		unlist(lapply( name, doIt ))
	}
)

#' @rdname getLinearSelection
setMethod('getLinearSelection', signature = c ('cellexalvrR', 'character'),
	definition = function ( cellexalObj, name ) {
	
	if ( length(name) > 1) {
		ret = unlist( lapply( name, function(d){ groupingInfo(cellexalObj, d) }))
		return (ret)
	}
	tmp = groupingInfo(cellexalObj,name)
	if ( class(tmp)[1] == 'cellexalvrR' ) {
		message("The grouping info was not found in the obejct!")
		browser()	
	}
	if ( class(tmp)[1] == "cellexalGrouping" && ! empty(tmp@linarObj)){
		tmp = tmp@linarObj
	}
	return(tmp)
	
	}
)


#' @rdname getLinearSelection
setMethod('getLinearSelection', signature = c ('cellexalvrR', 'cellexalLinear'),
	definition = function ( cellexalObj, name ) {
		name
	})
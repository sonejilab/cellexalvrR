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
	if( ! is.na(match( name, names(cellexalObj@usedObj$linearSelections)))){
		return (cellexalObj@usedObj$linearSelections[[name]])
	}
	tmp = groupingInfo(cellexalObj,name)
	if ( class(tmp)[1] == 'cellexalvrR' ) {
		stop("The grouping info was not found in the obejct!")
	}
	if ( class(tmp)[1] == "cellexalGrouping"){
		tmp = tmp@linarObj
	}
	if ( empty(tmp) ){
		stop(paste("There is no linear selection for the selection name '", name,"'!", sep=""))
	}
	return(tmp)
	
	}
)


#' @rdname getLinearSelection
setMethod('getLinearSelection', signature = c ('cellexalvrR', 'cellexalLinear'),
	definition = function ( cellexalObj, name ) {
		name
	})
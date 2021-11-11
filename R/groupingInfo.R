#' groupingInfo collects all information about one grouping from 
#' the cellexalvrR internals and returns them as a cellealGrouping object.
#' 
#' @name groupingInfo
#' @docType methods
#' @description  returns the information stored for the last grouping read
#' @param cellexalObj, cellexalvr object
#' @param gname The optional group name to get info on a specific grouping (not the last)
#' @title get information on a sample grouping
#' @return a cellexalGrouping object
#' @export 
#if ( ! isGeneric('groupingInfo') ){
setGeneric('groupingInfo', ## Name
	function ( cellexalObj, gname=NULL ) { 
		standardGeneric('groupingInfo') 
	}
)
#}


#' @rdname groupingInfo
setMethod('groupingInfo', signature = c ('cellexalvrR'),
	definition = function ( cellexalObj, gname=NULL ) {
	if ( is.null(gname)){
		gname = cellexalObj@usedObj$lastGroup
	}
	if ( !is.null( cellexalObj@groupSelectedFrom[[gname]])){
		if ( ncol(cellexalObj@groupSelectedFrom[[gname]]@linarObj@dat)==0){
			for ( name in names(cellexalObj@usedObj$linearSelections) ) {
				time = cellexalObj@usedObj$linearSelections[[name]]
				if ( time@gname == gname | time@parentSelection == gname){
					cellexalObj@groupSelectedFrom[[gname]]@linarObj = time
				}
			}
		}
		return ( cellexalObj@groupSelectedFrom[[gname]] )
	}

	message( "This object is not build up correctly - the session info is missing - creating it from scratch")

	if ( is.null(cellexalObj@groupSelectedFrom)) {
		cellexalObj@groupSelectedFrom = list()
	}
	if ( length(cellexalObj@drc) > 1 ) {
		message(paste( "More than one drc object - I assume you selected from",names(cellexalObj@drc)[1]," ;-)" ) )
	}
	
	grouping = NULL

	if ( ! is.na( match(gname, colnames(cellexalObj@userGroups)) ) ){
		grouping = cellexalObj@userGroups[,gname]
		order = as.integer(cellexalObj@userGroups[,paste(gname, 'order')])
		message(paste("the grouping", gname, "is not part of the cellexalvrR object" ) )

	}

	if ( is.null(grouping) ) {
		stop( paste("the grouping", gname, "is not found in this cellexalvrR object"))
	}

	if ( length(grouping) == ncol(cellexalObj@data) ){
		## the conversions and info loss is necessary to make this object work.
		if ( is.character( grouping )) {
			grouping = as.numeric(factor(grouping))
		}
		if ( is.factor(grouping) ){
			grouping = as.numeric( grouping)
		}

		names(grouping) = colnames(cellexalObj@data)
	}

	ginfo = new( 'cellexalGrouping',
		gname = gname,
		selectionFile= '',
		grouping = grouping ,
		order = order,
		drc = names(cellexalObj@drc)[1],
		col = rainbow( length(table(grouping)) )
	)

	cellexalObj@groupSelectedFrom[[gname]] = ginfo
	message( paste( sep="",
	"correct the returned object and put it there: cellexalObj@groupSelectedFrom[['",
	gname, "']]")
	)
	cellexalObj@groupSelectedFrom[[gname]]
} )

#' groupingInfo collects all information about one grouping from 
#' the cellexalvrR internals and returns them as a cellealGrouping object.
#' This function can also return a list of groupings and can process both 
#' cellexalLinear and cellexalGrouping classes as input. But not mixed stings and objects!
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
setMethod('groupingInfo', signature = c ('cellexalvrR', 'list'),
	definition = function ( cellexalObj, gname=NULL ) {
		if ( length(gname) > 1){
			groupingInfo( cellexalObj, unlist(lapply(gname, function(d){d@gname} ) ))
		}else {
			groupingInfo( cellexalObj, gname )
		}
	}
)




#' @rdname groupingInfo
setMethod('groupingInfo', signature = c ('cellexalvrR'),
	definition = function ( cellexalObj, gname=NULL ) {
	if ( is.null(gname)){
		gname = cellexalObj@usedObj$lastGroup
	}
	tmp = gname
	if ( length(gname) > 1) {
		## OK here we can only (1) get the entry for the second one - not add anything new!
		gname = gname[1]
	}
	if (! is.na(match( gname, names(cellexalObj@groupSelectedFrom) )) ){
		return ( cellexalObj@groupSelectedFrom[[gname]] )
	}
	for ( grouping in cellexalObj@groupSelectedFrom ){
		if ( ! empty(grouping@linarObj) ){
			if ( gname == grouping@linarObj@gname){
				return ( grouping@linarObj )
			}
		}
	}

	## one more option - do you have a file name as gname?!
	for ( group in cellexalObj@groupSelectedFrom ){
		if ( group@selectionFile == gname ){
			return ( group ) 
		}
	}

	if (!is.na(match ('linearSelections', names(cellexalObj@usedObj))) && ! is.na(match( gname, names(cellexalObj@usedObj$linearSelections) )) ){
		return ( cellexalObj@usedObj$linearSelections[[gname]] )
	}

	message( "The session info is missing - creating it from scratch")

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
		message(paste("grouping", gname, "has been fetched from the userGroups table" ) )

	}else if ( ! is.null( cellexalObj@usedObj$samples) ){
		if (! is.na( match(gname, colnames(cellexalObj@usedObj$samples)) )){
			if ( ncol(cellexalObj@userGroups) == 0){
				cellexalObj@userGroups = data.frame( gname = cellexalObj@usedObj$samples[,gname] )
				colnames(cellexalObj@userGroups) = gname
			}else {
				 cellexalObj@userGroups[,gname] = cellexalObj@usedObj$samples[,gname]
			}
			grouping = cellexalObj@usedObj$samples[,gname]
			## this has not been selected in VR - hence there is no grouping.
			order = cellexalObj@userGroups[,paste(gname, 'order')] = 1:ncol(cellexalObj@data)
			message(paste("grouping", gname, "has been fetched from the usedObj$samples table" ) )
		}
	}

	if ( is.null(grouping) ) {
		stop( paste("the grouping", gname, "is nowhere found in this cellexalvrR object"))
	}
    if ( ! is.factor(grouping) ){
		grouping = factor( grouping )
	}
	if ( length(grouping) == ncol(cellexalObj@data) ){
		## the conversions and info loss is necessary to make this object work.
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
	"correct the drc info: cellexalObj@groupSelectedFrom[['",
	gname, "']]@drc if ",cellexalObj@groupSelectedFrom[[gname]]@drc," is not correct" )
	)

	gname = tmp
	if (length( gname ) > 1){
		ret = unlist( lapply( gname, function(name) { cellexalObj@groupSelectedFrom[[name]]}) )
		return ( ret )
	}
	message(paste("You have obtained an updated cellexalvrR object - please re-run the function!" ) )
	cellexalObj
} )

#' saveGroupingInfocalls groupingInfo internally and returns both the cellexalObj and the grouping info
#' 
#' @name saveGroupingInfo
#' @docType methods
#' @description  returns the information stored for the last grouping read
#' @param cellexalObj, cellexalvr object
#' @param gname The optional group name to get info on a specific grouping (not the last)
#' @title get information on a sample grouping
#' @return a cellexalGrouping object
#' @export 
#if ( ! isGeneric('saveGroupingInfo') ){
setGeneric('saveGroupingInfo', ## Name
	function ( cellexalObj, gname=NULL ) { 
		standardGeneric('saveGroupingInfo') 
	}
)
#}

#' @rdname saveGroupingInfo
setMethod('groupingInfo', signature = c ('cellexalvrR', 'list'),
	definition = function ( cellexalObj, gname=NULL ) {
		if ( length(gname) > 1){
			saveGroupingInfo( cellexalObj, unlist(lapply(gname, function(d){d@gname} ) ))
		}else {
			saveGroupingInfo( cellexalObj, gname )
		}
	}
)


#' @rdname saveGroupingInfo
setMethod('saveGroupingInfo', signature = c ('cellexalvrR'),
	definition = function ( cellexalObj, gname=NULL ) {
		info = groupingInfo( cellexalObj, gname )
		if ( class(info)[1] == "cellexalvrR"){
			cellexalObj = info
			info <- groupingInfo( cellexalObj, cellidfile )
		}
		return (list( cellexalObj, info ))
 }
)
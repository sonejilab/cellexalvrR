#' The santity check for a cellexalGrouping object.
#' 
#' @name checkGrouping
#' @docType methods
#' @description check the cellexalGrouping
#' @param cellexalObj the cellexalvrR object
#' @param x an cellexalGrouping or a grouping name.
#' @title cellexalGrouping checks
#' @export 
#if ( ! isGeneric('checkGrouping') ){
setGeneric('checkGrouping', ## Name
	function (cellexalObj, x) { 
		standardGeneric('checkGrouping')
	}
)
#}


#' @rdname checkGrouping
setMethod('checkGrouping', signature = c ('cellexalvrR', 'character'),
	definition = function (cellexalObj, x) {
	x = groupingInfo( cellexalObj, x )
	checkGrouping ( cellexalObj, x )
})



#' @rdname checkGrouping
setMethod('checkGrouping', signature = c ('cellexalvrR', 'cellexalGrouping'),
	definition = function (cellexalObj, x) {
	x@error = ''
	if ( is.null( cellexalObj@userGroups[, x@gname])){
		x@error = c( x@error, paste("the grouping gname", x@gname,"is not defined in the cellexalObj"))
	}
	if ( is.null( cellexalObj@userGroups[, paste(x@gname, 'order')])){
		x@error = c( x@error, paste("the order info for gname", x@gname,"is not defined in the cellexalObj"))
	}
	if ( is.null( cellexalObj@drc[[x@drc]])){
		x@error = c( x@error, paste("the drc", x@drc,"is not defined in the cellexalObj"))
	}
	if ( ! all.equal( x@grouping, cellexalObj@userGroups[, x@gname]) == TRUE ){
		x@error = c( x@error, paste("my grouping is not the same as in the cellexalObj"))
	}

	if ( length(which(is.na(x@col))) > 0 ){
		#print ("there are NAs in the color info!")
		x@VRgrouping = x@grouping -1
		## best would be to change the grouping IDs to something useful here.
		goodIDS = 1:length(which(! is.na(x@col)))
		badIDs  = which(! is.na(x@col))
		# all.equal( x@grouping, cellexalObj@userGroups[, x@gname]) is true
		for ( id in goodIDS ){
			to.change = which(cellexalObj@userGroups[, x@gname] == badIDs[id])
			x@grouping[to.change] = id
		}
		cellexalObj@userGroups[, x@gname] = x@grouping
		x@col = x@col[badIDs]
		cellexalObj@groupSelectedFrom[[ x@gname ]] = x
		message("group IDs changed")
	}

	invisible(cellexalObj)
} )
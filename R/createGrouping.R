#' Create  new cellexalGrouping object and integrate it into the cellexal object
#' 
#' @name createGrouping
#' @docType methods
#' @description  returns the information stored for the last grouping read
#' @param cellexalObj, cellexalvr object
#' @param cells the cells in the order of selection 
#' @param group the group id for each cell (or the color in the VR process)
#' @param drc the drc the data was selected from
#' @param cellidfile the file that contained this info (will be created if NULL)
#' @param col a list with the color info for each group id (defaults to rainbow)
#' @title create a grouping object in the cellexalvrR object
#' @return an unpdated cellexalvrR object
#' @export 
#if ( ! isGeneric('createGrouping') ){
setGeneric('createGrouping', ## Name
	function ( cellexalObj, cells, groups, drc, cellidfile=NULL, col=NULL ) { 
		standardGeneric('createGrouping') 
	}
)
#}

#' @rdname createGrouping
setMethod('createGrouping', signature = c ('cellexalvrR', 'character'), 
	definition = function ( cellexalObj, cells, groups, drc, cellidfile=NULL, col=NULL ) {
		cellexalObj <- renew(cellexalObj)
		## check if the cellexalvrR object already contains this group
		cells2internOrder = match( cells, colnames(cellexalObj@data))
		internOrder2cells = match( colnames(cellexalObj@data), cells)
		internOrder2cells = internOrder2cells[which(!is.na(internOrder2cells))]

		if ( length (which( is.na( cells2internOrder))) > 0) {
			stop( "some (",length (which( is.na( cells2internOrder))),") cells are unknown!")
		}
		## here I need the oder columns values - which is the order in which the cells have been selected
		order = 1:length(cells)

		intern_group_is_the_same = which( unlist(lapply( grep( 'order', colnames( cellexalObj@userGroups), invert = TRUE ) ,
			function(id){
				## do we have the respective 'order' row?
				order_name = paste( colnames( cellexalObj@userGroups)[id], "order")
				if( is.na(match( order_name, colnames( cellexalObj@userGroups) )) ) { 
					return( FALSE ) 
				}
				# does the other group have the same cells selected
				group_n = length(which(!is.na(cellexalObj@userGroups[,id] )))
				if ( group_n != length(cells) ){
					return( FALSE ) 
				}
				# and finally - is the group order the same
				if ( all(cellexalObj@userGroups[internOrder2cells, order_name] == order )
					 & all(cellexalObj@userGroups[internOrder2cells, id] == groups )
					){
					return ( TRUE )
				}
			}
		)) == TRUE)

		if ( length( intern_group_is_the_same) == 1 ) {
			# we only need to set this as the last group
			cellexalObj@usedObj$lastGroup = colnames(cellexalObj@userGroups)[intern_group_is_the_same]
			return ( cellexalObj )
		}
		if ( length( intern_group_is_the_same) > 0 ) {
			stop("A library error - this should not have happened!")
		}

		## now the intern group does not exist and we need to create it!
		intern_group = rep(NA, ncol(cellexalObj@data) )
		intern_group[cells2internOrder] = groups
		names(intern_group) = colnames(cellexalObj@data)
		intern_order = rep(NA, ncol(cellexalObj@data) )
		intern_order[cells2internOrder] = order
		names(intern_order) = colnames(cellexalObj@data)

		id = length(cellexalObj@groupSelectedFrom) + 1
		gname = paste( sep=".", "User", "group", id)

		while ( !is.na(match( gname, colnames(cellexalObj@userGroups)))){
			id = id + 1
			gname = paste( sep=".", "User", "group", id)
		}
		if ( ncol(cellexalObj@userGroups) == 0) {
			cellexalObj@userGroups = data.frame(  as.factor(intern_group),intern_order )
			colnames(cellexalObj@userGroups ) = c( gname, paste(gname, 'order')  )

		}else {
			cellexalObj@userGroups[, gname] = as.factor( intern_group)
			cellexalObj@userGroups[, paste(gname, 'order')] = intern_order
		}
		
		if (is.null(col) ){
			col = as.character(	grDevices::rainbow( length( unique( groups ) ) ) )
		}

		ginfo = new( 'cellexalGrouping',
			gname = gname,
			grouping = as.factor(intern_group),
			order = as.factor(intern_order),
			'drc' = drc,
			col = col
		)
		intern_file = file.path( cellexalObj@usedObj$sessionPath, basename( cellidfile) )
		## the outfile needs to be in the ginfo object, too
		if ( is.na( cellidfile )  ){
			fname = file.path(cellexalObj@outpath, paste( sep="",'selection', id, ".txt") )
			while ( file.exists( fname) ){
				id = id+1
				fname = file.path(cellexalObj@outpath, paste( sep="",'selection', id, ".txt") )
			}
			cellidfile = fname
		}

		if ( ! file.exists( intern_file ) ) {
			ginfo@selectionFile=basename( intern_file )
			exportSelection( ginfo, intern_file )
		}
		else {
			# the file is not NULL and exists
			intern_file = file.path( cellexalObj@outpath, basename( cellidfile) )
			if ( ! file.exists( intern_file )){
				file.copy( cellidfile, intern_file)
			}
			ginfo@selectionFile =basename( cellidfile )
		}

		## and finally we store the group info
		cellexalObj@groupSelectedFrom[[ginfo@gname]] = ginfo
		cellexalObj@usedObj$SelectionFiles[[ginfo@gname]] = intern_file
		cellexalObj@colors[[ ginfo@gname ]] = ginfo@col
		cellexalObj@usedObj[['lastGroup']] = ginfo@gname

		invisible (cellexalObj)
	}
)


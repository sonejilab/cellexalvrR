#' This function converts a selection described in a userGroups column to a VR selection file.
#'
#' @name export_UserGrouping
#' @docType methods
#' @description export a list of genes of interest to be loaded as heatmaps in cellexalVR
#' @param x the cellexal object
#' @param colname the userGrouping column name
#' @param outfile the out file
#' @param colorF a function returning the colors in the order they should appear on the heatmap (default = rainbow)
#' @title export a list of genes of interest to be loaded as heatmaps in cellexalVR
#' @export 
#if ( ! isGeneric('export_UserGrouping') ){
setGeneric('export_UserGrouping', ## Name
	function (x, colname, outfile, colorF = rainbow ) { 
		standardGeneric('export_UserGrouping')
	}
)
#}



#' @rdname export_UserGrouping
setMethod('export_UserGrouping', signature = c ('cellexalvrR'),
	definition = function (x, colname, outfile, colorF = rainbow ) {
	if ( is.null( x@userGroups[,colname] )){
		stop(paste("Grouping", colname,"not in the object!") )
	}
	
	OK = which( !is.na(x@userGroups[,colname]))
	if ( length(OK) == 0 ) {
		stop(paste("No annotated cells using colname", colname))
	}
	new= x@userGroups[OK,]
	if ( ! is(new[,colname], 'factor' )){
		new[,colname]= factor( new[,colname], labels=sort(unique(new[,colname])))
	}
	#new= new[order(new[,colname]),]
	color=colorF( length( levels(new[,colname])) )
	new = data.frame( colnames(x@data)[OK], color[new[,colname]], names(x@drc)[1], new[,colname] )
	new= new[order(new[,4]),]
	new = cbind(new, 1:nrow(new) )
	
	## the drc the groups have been selected from is unknown - take the first one...
	# name color drcName id - no colnames!
	utils::write.table( new, file= outfile, quote=FALSE, 
		row.names=FALSE, sep="\t", col.names=FALSE)
	invisible(x)
} )

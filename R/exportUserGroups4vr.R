	
#if ( ! isGeneric('renew') ){
setGeneric('exportUserGroups4vr', ## Name
	function ( cellexalObj, path ) { 
		standardGeneric('exportUserGroups4vr') 
	}
)
#}

#' This is a 'VR' helper function that creates a dynamic file used to make the R groupings known to the VR process.
#'
#' Users of the 'cellexalvrR' R class should not need this function.
#' @name exportUserGroups4vr
#' @aliases exportUserGroups4vr,cellexalvrR-method
#' @rdname exportUserGroups4vr-methods
#' @docType methods
#' @description  Creates a summary file for the vr process Creates a file groupings_info.txt in the
#' @description  outfolder that contains the group name (in the R object) the numer of groups in the
#' @description  selection and the number of cells in the whole group.
#' @param cellexalObj, cellexalvr object
#' @param path the outpath
#' @title VR helper function exportUserGroups4vr
#' @examples
#' dir.create('data')
#' exportUserGroups4vr( cellexalObj, 'data') #function definition in file 'exportUserGroups4vr.R'
#' @export exportUserGroups4vr
setMethod('exportUserGroups4vr', signature = c ('cellexalvrR'),
	definition = function ( cellexalObj, path ) {
	#cellexalObj <- loadObject(cellexalObj) #function definition in file 'lockedSave.R'
	
	names <- colnames(cellexalObj@userGroups) [grep('order', colnames(cellexalObj@userGroups), invert=T)]
	## create the grouping files for the VR process?
	for ( gname in names ) {
		fname = paste(paste( unlist(stringr::str_split( gname, '\\s+')), collapse='_'),'cgr', sep='.' )
		fname = file.path( path,fname )
		if ( ! file.exists( fname ) ) {
			## create the grouping info for the VR process
			# no header
			# HSPC_639    #FF0000    DDRTree    0
			ids =  which( is.na(cellexalObj@userGroups[,gname]) == F)
			col = cellexalObj@colors[[gname]]
			if ( is.null(col) ){
				col = cellexalObj@colors[[gname]] <- grDevices::rainbow( length(unique( as.integer( cellexalObj@userGroups[ids,gname] ) )))
			}
			drc = 'unknown'
			try({drc =  groupingInfo(cellexalObj, gname)$drc},silent=TRUE)
			gid = NULL
			if (drc != 'unknown' ) {
				gid = as.integer( cellexalObj@userGroups[ids,gname] ) -1 
			}else {
				gid = as.integer( cellexalObj@userGroups[ids,gname] )
			}

			t <- data.frame( 
				cellname = colnames(cellexalObj@data)[ids],
				color = cellexalObj@colors[[gname]][  cellexalObj@userGroups[ids,gname]  ],
				'parent.graph' = rep( drc, length(ids) ),
				'gid' = gid
			)
			utils::write.table( t, col.names=F, row.names=F, quote=F, file=fname, sep="\t" )
		}
	}
	
	#groupN <- unlist(lapply( names, function(n) { length(cellexalObj@colors[[n]]) } ) )
	#groupCount <- unlist(lapply( names, function(n) { length( which( is.na(cellexalObj@userGroups[,n]) == F)) } ) )
	#ret <- cbind(  'group name' = names, 'groups [n]' =  groupN, 'cells [n]' = groupCount )
	#utils::write.table(ret, file=file.path( path, 'groupings_info.txt'), row.names=F, col.names=T, sep="\t", quote=F )
	
	#cellexalObj@outpath = path
	#lockedSave( cellexalObj, path ) #function definition in file 'lockedSave.R'
	
} )

#' @describeIn exportUserGroups4vr cellexalvrR
#' @docType methods
#' @description simply preload the 'cellexalObj' before running the 'cellexalvrR' specififc function
#' @param cellexalObj the cellexalObj.RData file
#' @param path the outpath
#' @title load object and run exportUserGroups4vr
#' @export exportUserGroups4vr
setMethod('exportUserGroups4vr', signature = c ('character'),
		definition = function (cellexalObj, path) {
			## This is the first function the VR does actually run in the R environment.
			if ( file.exists( file.path(path, 'cellexalObj.RData'))){
				cellexalObj <- loadObject(file.path(path, 'cellexalObj.RData')) #function definition in file 'lockedSave.R'
			}else {
				cellexalObj <- loadObject(cellexalObj) #function definition in file 'lockedSave.R'
			}
			exportUserGroups4vr( cellexalObj, path ) #function definition in file 'exportUserGroups4vr.R'
		}
)

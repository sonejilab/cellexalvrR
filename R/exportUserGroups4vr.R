#' This is a 'VR' helper function that creates a dynamic file used to make the R groupings known to the VR process.
#'
#' Users of the 'cellexalvrR' R class should not need this function.
#' @name exportUserGroups4vr
#' @docType methods
#' @description  Creates a file groupings_info.txt in the
#' @description  outfolder that contains the group name (in the R object) the numer of groups in the
#' @description  selection and the number of cells in the whole group.
#' @param cellexalObj, cellexalvr object
#' @param path the outpath
#' @title VR helper function exportUserGroups4vr
#' @examples
#' \dontrun{
#' dir.create('data')
#' exportUserGroups4vr( cellexalObj, 'data')
#' }
#' @export 
#if ( ! isGeneric('exportUserGroups4vr') ){
setGeneric('exportUserGroups4vr', ## Name
	function ( cellexalObj, path ) { 
		standardGeneric('exportUserGroups4vr') 
	}
)
#}



#' @rdname exportUserGroups4vr
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
			info = groupingInfo(cellexalObj, gname )
			ids = which(! is.na(info@grouping))
			if ( length(info@VRgrouping) == 0){
				info@VRgrouping = info@grouping -1
			}
			t <- data.frame( 
				cellname = colnames(cellexalObj@data)[ids],
				color = info@col[ info@grouping[ids] ],
				'parent.graph' = rep( info@drc, length(ids) ),
				'gid' = info@VRgrouping[ids] 
			)
			t = t[order(info@order[ids]),]
			utils::write.table( t, col.names=F, row.names=F, quote=F, file=fname, sep="\t" )
		}
	}
} )


#' @rdname exportUserGroups4vr
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

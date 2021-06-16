#' A thread save function to save cellexalvrR objects. 
#' Needed for the VR interaction.
#' @name lockedSave
#' @docType methods
#' @description  A thread save saving of the object. 
#' @param cellexalObj, cellexalvr object
#' @param path the output path
#' @param what which part needs saving? (default NULL == all)
#' @title thread save save method for cellexalvrR objects
#' @keywords lockedSave
#' @export lockedSave
#if ( ! isGeneric('lockedSave') ){
setGeneric('lockedSave', ## Name
	function (cellexalObj, path=NULL, what=NULL ) {
		standardGeneric('lockedSave')
	}
)
#}


#' @rdname lockedSave
setMethod('lockedSave', signature = c ('cellexalvrR'),
	definition = function (cellexalObj, path=NULL ) {
		if ( is.null(path) ){
			path= cellexalObj@outpath
		}else if ( ! methods::.hasSlot(cellexalObj, "outpath") ) {
			cellexalObj@outpath = path
		}
	ofile = file.path( path, 'cellexalObj.RData' )
	lockFile = file.path( paste(ofile, 'lock', sep= '.'))
	while ( file.exists(lockFile) ){
		Sys.sleep(1)
	}
	file.create(lockFile)
	save(cellexalObj, file=ofile)
	## and now I should remove all parts...
	cleanParts ( path ) #function definition in file 'integrateParts.R'
	file.remove(lockFile)
	
	#print (paste("saved the object to",path))
} )


#' loadObject has thread functionallity looking for a lock file and waiting for 'maxwait' seconds 
#' before reporting a failed attempt.
#' 
#' @name loadObject
#' @docType methods
#' @description Loads the cellexalvr object or returns the cellexalvrR object.
#' @param fname the file to load or a cellexalvr object
#' @param maxwait stop after maxwait seconds default=50
#' @title thread save load function for cellexalvrR obejcts
#' @export 
#if ( ! isGeneric('loadObject') ){
setGeneric('loadObject', ## Name
	function ( fname, maxwait=50 ) { 
		standardGeneric('loadObject') 
	}
)
#}




#' @rdname loadObject
setMethod('loadObject', signature = c ('cellexalvrR'),
		definition = function ( fname, maxwait=50 ) {
			return (fname)
} )


#' @rdname loadObject
setMethod('loadObject', signature = c ('character'),
		definition = function ( fname, maxwait=50 ) {
			if ( file.exists( fname) ) {
				waited = 0
				while ( file.exists( paste(fname, 'lock',sep='.'))){
					Sys.sleep(1)
					waited = waited +1
					if ( waited == maxwait) { break }
				}
				if (waited != maxwait ){
					load(fname)
				}else {
					stop( paste("Could not obtain access to locked file", fname ))
				}
			}else {
				stop( paste( "file does not exist", fname) )
			}
			if ( ! is.null(attributes(cellexalObj@class)$package) ) {
				if ( attributes(cellexalObj@class)$package == 'cellexalvr' ){
					class(cellexalObj) = 'cellexalvrR'
					cellexalObj = renew(cellexalObj) #function definition in file 'renew.R'
				}
			}
			## old objects need an update
			if ( ! methods::.hasSlot( cellexalObj, 'data') ){
				new = MakeCellexalVRObj ( cellexalObj@data, drc.list = cellexalObj@drc,	specie=cellexalObj@specie,cell.metadata= cellexalObj@meta.cell, facs.data= NULL ) #function definition in file 'makeCellexalVRObj.R'
				new@userGroups = cellexalObj@userGroups
				new@colors = cellexalObj@colors
				new@groupSelectedFrom = cellexalObj@groupSelectedFrom
				new@userGroups = cellexalObj@userGroups
				new@usedObj = cellexalObj@usedObj
				new@tfs = cellexalObj@tfs
				new@index = cellexalObj@index
				rm(cellexalObj)
				cellexalObj = new
				rm(new)
				gc()
			}
			## this is the function the VR uses to load an object.
			## and we need to check if our drc names make sense!
			drcFiles = list.files( dirname( fname ), full.names = TRUE, pattern="*.mds" )
			
			if ( length( drcFiles ) > 0) {
				fnames = unlist( lapply( drcFiles, basename) )
				fnames = str_replace_all( fnames, '.mds$', '' )
				m = match(names(cellexalObj@drc), fnames )
				if ( length(m) != length(fnames) | length(which(is.na(m))) > 0 ) {
					message("The drc names between VR and R do not overlap - updating the R object!")
					cellexalObj@drc = lapply(drcFiles, function(n){ 
						d = utils::read.delim( n, header=F )
						if ( d[1,1] == 'CellID' ){
							colnames(d) = d[1,]
							d= d[-1,]
						}else {
							if ( ncol(d) == 4 ) {
								colnames(d) = c('CellID','dim1','dim2','dim3')
							}else if (ncol(d) == 7 ) {
								colnames(d) = c('CellID','dim1','dim2','dim3','velo1','velo2','velo3' )
							}else {
								stop( paste("The file", n,"has the wrong format!"))
							}
						}
						rownames(d) = d[,1]
						d= d[,-1]
						m = match(colnames(cellexalObj@data), rownames(d))
						d=d[m,]
						rownames(d) = colnames(cellexalObj@data)
						d
					} )

					names(cellexalObj@drc) = fnames
					message(paste("Saving the updated R object to", dirname( fname ) ) )
					lockedSave(cellexalObj, dirname( fname ) )
				}
			}
			
			#tmp = new('cellexalvrR')
			#reload = 0
			if ( ! file.exists(cellexalObj@outpath )) {
				cellexalObj@outpath = normalizePath(dirname( fname ))
			}else {
				cellexalObj@outpath = normalizePath(cellexalObj@outpath)
			}
			## there might be different other objects in the same path
			## integrat them now
			cellexalObj = integrateParts( cellexalObj , normalizePath(dirname( fname )) ) #function definition in file 'integrateParts.R'
			colnames(cellexalObj@userGroups) = stringr::str_replace_all( colnames(cellexalObj@userGroups), '.order', ' order')
			cellexalObj
		} )


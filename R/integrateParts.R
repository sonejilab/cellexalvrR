#' The function integrateParts is one main part in the load and save speed up implemented for cellexalvrR.
#'
#' Instead of saving all data in each VR script call only the modified data is saved.
#' This function loads all separately saved parts into the main cellexalvrR object.
#' @name integrateParts
#' @docType methods
#' @description CellexalvrR object can be saved partially. This function loads them.
#' This function is called by the loadObject function. Nothing the user has to think about! 
#' @param x the cellexalObj to add to
#' @param path the path where the parts are stored (normally the @outpath)
#' @title integrate the parts that have been split from the main object
#' @export 
#if ( ! isGeneric('integrateParts') ){
setGeneric('integrateParts', ## Name
			function ( x , path=NULL ) { 
				standardGeneric('integrateParts')
			}
)
#}



#' @rdname integrateParts
setMethod('integrateParts', signature = c ('cellexalvrR'),
	  definition = function ( x , path=NULL ) {
			if ( is.null(path) )
				path = x@outpath
			## now we check a list of outpath slots that could be updataed:
			## if you change somthing here also change all other functions in this file!
			F = c( 'meta.cell',  'meta.gene',  'userGroups',  'usedObj', 'groupSelectedFrom', 'colors', 'lastGroup' )
			sample  = annotation = userGroups = usedObj = groupSelectedFrom = colors = lastGroup = NULL 
			for ( i in 1:length(F)  ) {
				
				filename = partFname( F[i] , path ) #function definition in file 'integrateParts.R'
				
				if ( file.exists( filename ) ){
					#print ( paste("I am trying to load file ", filename ))
					
					load( filename )
					if (i == 1) {#sample.RData 
						x@meta.cell = sample
					}else if ( i == 2) {
						x@meta.gene = annotation
					}else if ( i == 3) {
						x@userGroups = userGroups
					}else if ( i == 4) {
						x@usedObj = usedObj
					}else if ( i==5) {
						x@groupSelectedFrom = groupSelectedFrom
					}else if ( i == 6 ) {
						x@colors = colors
					}else if ( i == 7 ) {
						x@usedObj$lastGroup = lastGroup
					}
				}
			}
			invisible( x )
		} )

#' The function savePart is one main part in the load and save speed up implemented for cellexalvrR
#'
#' Instead of saving all data in each VR script call only the modified data is saved.
#' This function saves a specific data part of the whole cellexalvrR speeding up the save process.
#' @name savePart
#' @docType methods
#' @description save only a part of the cellexal object increasing the VR interaction speed
#' @param x  the cellexalvrR object
#' @param part one of tge slots in cellexalvrR
#' @param path an optional outpath (default x@outpath)
#' @title save only a slot of the cellexalObj
#' @export 
#if ( ! isGeneric('savePart') ){
setGeneric('savePart', ## Name
			function ( x, part = c( 'meta.cell',  'meta.gene',  'userGroups',  'usedObj', 'groupSelectedFrom', 'colors', 'lastGroup' ), path=NULL ) { 
				standardGeneric('savePart')
			}
)
#}


#' @rdname savePart
setMethod('savePart', signature = c ('cellexalvrR'),
		definition = function ( x, part = c( 'meta.cell',  'meta.gene',  'userGroups',  'usedObj', 'groupSelectedFrom', 'colors', 'lastGroup' ), path =NULL ) {
			#meta.cell meta.gene userGroups usedObj
			if ( is.null(path) )
				path = x@outpath
			
			filename = partFname( part, path ) #function definition in file 'integrateParts.R'
			#print ( paste("I am saving parts file", filename ))
			if (part == 'meta.cell') {#sample.RData
				sample = x@meta.cell
				save( sample, file=filename)
			}else if ( part == 'meta.gene') {
				annotation = x@meta.gene
				save( annotation, file=filename)
			}else if ( part == 'userGroups') {
				userGroups = x@userGroups
				save( userGroups, file=filename)
			}else if ( part == 'usedObj') {
				usedObj = x@usedObj
				save( usedObj, file=filename)
			}else if ( part == 'groupSelectedFrom') {
				groupSelectedFrom = x@groupSelectedFrom
				save( groupSelectedFrom, file=filename)
			}else if ( part == 'colors') {
				colors = x@colors
				save( colors, file=filename)
			}else if ( part == 'lastGroup') {
				lastGroup = x@usedObj$lastGroup
				save( lastGroup, file=filename)
			}
			
			invisible( x )
		} )


#' The function partFname checks which part to save and throws an error if the part is not avaialable for saving.
#' 
#' @name partFname
#' @docType methods
#' @description returns the possible outfile for the partial saving of the cellexal object
#' @param part to get the filename for (  'meta.cell',  'meta.gene',  'userGroups',  'usedObj', 'all' )
#' @param path the outpath
#' @title create the filename fir a cellexalvrR slot only save
#' @export 
#if ( ! isGeneric('renew') ){
setGeneric('partFname', ## Name
			function ( part = c( 'meta.cell',  'meta.gene',  'userGroups',  'usedObj', 'groupSelectedFrom', 'colors', 'lastGroup', 'all' ), path ) { 
				standardGeneric('partFname')
			}
)
#}



#' @rdname partFname
setMethod('partFname', signature = c ('character'),
		definition = function ( part = c( 'meta.cell',  'meta.gene',  'userGroups',  'usedObj', 'groupSelectedFrom', 'colors', 'lastGroup', 'all' ), path ) {
			F = c( 'sample.RData', 'usergroups.RData', 'annotation.RData', 'usedObj.RData' )
			fname=NULL
			if (part == 'meta.cell') {#sample.RData
				fname = 'sample'
			}else if ( part == 'meta.gene') {
				fname = 'annotation'	
			}else if ( part == 'userGroups') {
				fname = 'userGroups'
			}else if ( part == 'usedObj') {
				fname = 'usedObj'
			}else if ( part == 'groupSelectedFrom') {
				fname = 'groupSelectedFrom'
			}else if ( part == 'colors') {
				fname = 'colors'
			}else if ( part == 'lastGroup') {
				fname = 'lastGroup'
			}else if ( part == 'all') {
				fname = unlist( lapply( c( 'meta.cell',  'meta.gene',  'userGroups',  'usedObj', 'groupSelectedFrom', 'colors', 'lastGroup'), partFname, path ))
				return( fname )
			}
			else {
				stop( paste( "This part can not be saved:",part ))
			}
			
			unlist( lapply(fname, function( FN ) { file.path( path, paste(sep="",'.' , FN, '.RData' ) )  } ) )
		} )

#' The parts clean up function is called after a object is saved using the lockedSave function,
#' as all previousely saved sub parts should have been integrated into this object then.
#' 
#' @name cleanParts
#' @docType methods
#' @description remove all parts from the file system
#' @param path the outpath
#' @title cleanParts removes all parts when storing the whole object.
#' @export 
#if ( ! isGeneric('renew') ){
setGeneric('cleanParts', ## Name
			function ( path ) { 
				standardGeneric('cleanParts')
			}
)
#}


#' @rdname cleanParts
setMethod('cleanParts', signature = c ('character'),
		definition = function ( path ) {
			#print ( paste(path, "I am cleaning the files:", paste( collapse=", ", partFname( 'all', path )))) #function definition in file 'integrateParts.R'
			for ( fname in partFname( 'all', path ) ) { #function definition in file 'integrateParts.R'
				if ( file.exists(fname))
					unlink( fname)
			}
		}
)

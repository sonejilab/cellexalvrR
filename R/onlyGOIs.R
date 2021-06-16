#' onlyGOIs is able to use the inbuild GOI datasest to subset the main data to e.g. TFs only.
#' 
#' This function is used internally.
#' 
#' @name onlyGOIs
#' @docType methods
#' @description  Allows the user to select only (G)enes (O)f (I)nterest lists from the object
#' @param cellexalObj, cellexalvr object
#' @param name the name of the GIO list (eg TFs or epigenetic)
#' @title Non VR method to reduce a cellexalvrR object to only include specififc genes
#' @export 
#if ( ! isGeneric('onlyGOIs') ){
setGeneric('onlyGOIs', ## Name
	function ( cellexalObj, name ) { 
		standardGeneric('onlyGOIs') 
	}
)
#}


#' @rdname onlyGOIs
setMethod('onlyGOIs', signature = c ('cellexalvrR'),
	definition = function ( cellexalObj, name ) {
	if ( is.na( match(name, colnames(cellexalObj@meta.gene)))) {
		tryCatch({
					cellexalObj = useInbuiltGOIlists( cellexalObj, name) }, error= {  #function definition in file 'useInbuiltGOIlists.R'
					stop( "Sorry, but this GIO list not known" )
				} )
	}
	cellexalObj = reduceTo( #function definition in file 'reduceTo.R'
			cellexalObj, 
			'row', 
			to=rownames(cellexalObj@data)[which(is.na(as.vector(cellexalObj@meta.gene[,name]))==F)] 
	)

	cellexalObj
} )

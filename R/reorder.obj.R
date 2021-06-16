#' reorder function to reorder the whole cellexalvrR object based on a cell order.
#' @name reorderSamples
#' @docType methods
#' @description  this function reorderes the cellexalvr object based on a column in the annotation
#' @description  table (e.g. for plotting)
#' @param dataObj the cellexalvr object
#' @param column the annotation column to reorder on
#' @title simple reordering of the samples
#' @export
#if ( ! isGeneric('renew') ){
setGeneric('reorderSamples', ## Name
	function ( dataObj, column ) { 
		standardGeneric('reorderSamples') 
	}
)
#}



#' @rdname reorderSamples
setMethod('reorderSamples', signature = c ('cellexalvrR'),
	definition = function ( dataObj, column ) {
	if (! is.na( match ( column, colnames(dataObj@meta.cell)) ) ) {
		ids = order( dataObj@meta.cell[,column])
	}else {
		ids = order( dataObj@userGroups[,column])
	}
	dataObj@data <- dataObj@data[ , ids]
	if ( ncol(dataObj@data) == nrow(dataObj@meta.cell) ) {
		dataObj@meta.cell <- dataObj@meta.cell[ids,]
	}
	if ( ncol(dataObj@data) == nrow(dataObj@userGroups) ) {
		dataObj@userGroups <- dataObj@userGroups[ids,]
	}
	for ( n in names(dataObj@drc) ) {
		#browser()
		if ( ! is.null(rownames(dataObj@drc[[n]]))){
			#if ( n == 'LargeSubset'){browser()}
			want = rownames(dataObj@meta.cell)[ids]
			here <- match(want, rownames(dataObj@drc[[n]]))
			if ( length(here) > 0 ){
				#here = here[which(!is.na(here))]
				get = want[which(!is.na(here))]
				idsHere =  match(get, rownames(dataObj@drc[[n]]))
			}
			else {
				idsHere = c()
			}
			result = tryCatch({
   			 	dataObj@drc[[n]]  = dataObj@drc[[n]][idsHere,]
				}, 
				error = function(error_condition) {
   				if(interactive()) { browser() }
			} )
			
		}
		else if (nrow(dataObj@drc[[n]]) == 0) {
			dataObj@drc[[n]] = dataObj@drc[[n]]
		}
		else {
   			dataObj@drc[[n]] = dataObj@drc[[n]][ids,]
		}
	}
	for ( n in names(dataObj@groupSelectedFrom) ) {
		dataObj@groupSelectedFrom[[n]]@order= dataObj@groupSelectedFrom[[n]]@order[ids]
		dataObj@groupSelectedFrom[[n]]@grouping= dataObj@groupSelectedFrom[[n]]@grouping[ids]
	}
	dataObj = check(dataObj)
	if ( ! is.null (dataObj@usedObj$timelines)) {
		for ( name in names( dataObj@usedObj$timelines ) ) {
			if ( length(dataObj@usedObj$timelines[[name]]@error) > 0 ){
				warning( paste("timeline", name,"is invalid - removed!"))
				dataObj@usedObj$timelines[[name]] = NULL
			}
		}
	}
	if ( !dataObj@usedObj$checkPassed ) {
		if(interactive()) { browser() }
	}
	invisible(dataObj)
} )


#' reorder function to reorder the whole cellexalvrR object based on a genes order.
#' @name reorderGenes
#' @docType methods
#' @description  this function reorderes the cellexalvr object based on a column in the samples table
#' @description  (e.g. for plotting)
#' @param dataObj the cellexalvr object
#' @param column the samples column to reorder on
#' @title simple reordering of the genes
#' @export
#if ( ! isGeneric('renew') ){
setGeneric('reorderGenes', ## Name
	function ( dataObj, column ) { 
		standardGeneric('reorderGenes') 
	}
)
#}


#' @rdname reorderGenes
setMethod('reorderGenes', signature = c ('cellexalvrR'),
	definition = function ( dataObj, column ) {
	dataObj@data <- dataObj@data[ order( dataObj@meta.gene[,column]),]
	dataObj@meta.gene <- dataObj@meta.gene[order( dataObj@meta.gene[,column]),]
	dataObj
} )

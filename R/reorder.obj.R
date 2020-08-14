#' reorder function to reorder the whole cellexalvrR object based on a samples order.
#' @name reorder.samples
#' @aliases reorder.samples,cellexalvrR-method
#' @rdname reorder.samples-methods
#' @docType methods
#' @description  this function reorderes the cellexalvr object based on a column in the annotation
#' @description  table (e.g. for plotting)
#' @param dataObj the cellexalvr object
#' @param column the annotation column to reorder on
#' @title simple reordering of the samples
#' @export
if ( ! isGeneric('reorder.samples') ){setGeneric('reorder.samples', ## Name
	function ( dataObj, column ) { 
		standardGeneric('reorder.samples') 
	}
) }

setMethod('reorder.samples', signature = c ('cellexalvrR'),
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
			#if ( n == 'rna_pca'){browser()}
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
   				 browser()
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
		dataObj@groupSelectedFrom[[n]]$order= dataObj@groupSelectedFrom[[n]]$order[ids]
		dataObj@groupSelectedFrom[[n]]$grouping= dataObj@groupSelectedFrom[[n]]$grouping[ids]
	}
	dataObj = check(dataObj)
	if ( !dataObj@usedObj$checkPassed ) {
		browser()
	}
	dataObj
} )

#' reorder function to reorder the whole cellexalvrR object based on a genes order.
#' @name reorder.genes
#' @aliases reorder.genes,cellexalvrR-method
#' @rdname reorder.genes-methods
#' @docType methods
#' @description  this function reorderes the cellexalvr object based on a column in the samples table
#' @description  (e.g. for plotting)
#' @param dataObj the cellexalvr object
#' @param column the samples column to reorder on
#' @title simple reordering of the genes
#' @export
if ( ! isGeneric('reorder.genes') ){setGeneric('reorder.genes', ## Name
	function ( dataObj, column ) { 
		standardGeneric('reorder.genes') 
	}
) }

setMethod('reorder.genes', signature = c ('cellexalvrR'),
	definition = function ( dataObj, column ) {
	dataObj@data <- dataObj@data[ order( dataObj@meta.gene[,column]),]
	dataObj@meta.gene <- dataObj@meta.gene[order( dataObj@meta.gene[,column]),]
	dataObj
} )

#' @name pseudoBulkCells
#' @docType methods
#' @description create a pseudo bulk dataset from a spare matrix.
#' @param cellexalObj if x is a cellexalLinear object this is necessary to create the zscored matrix.
#' @param minSize the minimum size of a group (default 10 (cells))
#' @param minBulks the minimal number of pseudo bulk samples (default 10)
#' @param maxBulks the maximum number of pseudo bulk samples (default 500)
#' @title merge cells based on linear order
#' @returns a sparse matrix summed up ba mean.
#' @export 
#if ( ! isGeneric('renew') ){
setGeneric('pseudoBulkCells', ## Name
	function ( cellexalObj, grouping, minSize=10, minBulks = 10, maxBulks = 500 ) { 
		standardGeneric('pseudoBulkCells')
	}
)
#}


#' @rdname pseudoBulkCells
setMethod('pseudoBulkCells', signature = c ('cellexalvrR', 'cellexalGrouping' ),
definition = function (  cellexalObj, grouping, minSize=10, minBulks = 10, maxBulks = 500 ) {

	res = pseudoBulk(  grouping, cellexalObj, minSize=minSize, 
		minBulks = minBulks, maxBulks = maxBulks)

	# for (tName in names(cellexalObj@usedObj$linearSelections)[-1]) {
	# 	time = cellexalObj@usedObj$linearSelections[[tName]]
	# 	n = nrow(time@dat)
	# 	loc = reduceTo( cellexalObj, what='col', to=rownames(time@dat))
	# 	if ( n > 1000 ){
	# 		ids = rep( 1:1000, floor(n/1000))
	# 		ids = c( ids, sample( 1:1000, n%%1000))
	# 		ids = sort(ids)
	# 		B = FastWilcoxTest::collapse( loc@data, ids, 2 )
	# 		B <- Matrix::Matrix(B, sparse = TRUE)
	# 		rownames(B) = rownames(cellexalObj@data)
	# 		colnames(B) = 1:1000
	# 		cellexalObj@usedObj$linearSelections[[tName]]@geneClusters[['collapsedExp']] = B
	# 	}else {
	# 		cellexalObj@usedObj$linearSelections[[tName]]@geneClusters[['collapsedExp']] = loc@data
	# 	}
		
	# }
	invisible(cellexalObj)
})
#' Allows the user to define (G)enes (O)f (I)nterest lists in the object
#' @param cellexalObj A cellexalvr object
#' @param name the name of the GIO list (eg TFs or epigenetic)
#' @param genes a list of gene symbols that match to the @data rownames
#' @export defineGOIs
defineGOIs <- function( cellexalObj, name, genes ) {
	if (nrow(cellexalObj@meta.gene)==0) {
		cellexalObj@meta.gene <- matrix(ncol=2, c(rownames(cellexalObj@data), rep( 0, nrow(cellexalObj@data)) ) )
		colnames(cellexalObj@meta.gene) = c('Gene Symbol', 'useless')
		rownames(cellexalObj@meta.gene) = rownames(cellexalObj@data)
	}
	if ( ! is.na( match(name, colnames(cellexalObj@meta.gene)))) {
		stop( "Sorry, but this GIO list has already been defined" )
	}
	cellexalObj@meta.gene <- cbind( cellexalObj@meta.gene, is.na(match( rownames(cellexalObj@data), genes)) == F )
	colnames(cellexalObj@meta.gene)[ncol(cellexalObj@meta.gene)] = name
	cellexalObj@usedObj$GOIs = c( cellexalObj@usedObj$GOIs, name)
	
	invisible(cellexalObj)
}
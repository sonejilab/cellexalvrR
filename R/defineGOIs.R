#' CellexalvrR includes several tables describing gene functionality.
#' Namely TFs, epigenetic, CellCycle and CellSurface genes.
#' This function will match the CellexalvrR object against these lists and add a
#' column into the meta.gene table containing 0 and 1 values.
#'
#' This function is used for the correlation networks in VR.
#' @name defineGOIs
#' @docType methods
#' @description  Allows the user to define (G)enes (O)f (I)nterest lists in the object
#' @param cellexalObj, cellexalvr object
#' @param name the name of the GIO list (eg TFs or epigenetic)
#' @param genes a list of gene symbols that match to the @data rownames
#' @param lables a list of lables for the GIO column (default NULL)
#' @title Define special genes based on inbuilt data sets (TFs, ...)
#' @export
#if ( ! isGeneric('renew') ){
setGeneric('defineGOIs', ## Name
	function ( cellexalObj, name, genes, lables=NULL ) { 
		standardGeneric('defineGOIs') 
	}
)
#}


#' @rdname defineGOIs
setMethod('defineGOIs', signature = c ('cellexalvrR'),
	definition = function ( cellexalObj, name, genes, lables=NULL ) {
			if ( is.null(lables) ) {
				lables = rep(name, length(genes))
			}
			if (nrow(cellexalObj@meta.gene)==0) {
				cellexalObj@meta.gene <- matrix(ncol=2, c(rownames(cellexalObj@data), rep( 0, nrow(cellexalObj@data)) ) )
				colnames(cellexalObj@meta.gene) = c('Gene Symbol', 'useless')
				rownames(cellexalObj@meta.gene) = rownames(cellexalObj@data)
			}
			if ( ! is.na( match(name, colnames(cellexalObj@meta.gene)))) {
				stop( "This GIO list has already been defined" )
			}
			
			#OK <- which(is.na(match( rownames(cellexalObj@data), genes)) == F)
			OK_genes <- match ( genes, rownames(cellexalObj@data) )
			OK <- OK_genes[which(is.na(OK_genes) ==F)]
			n = rep(NA, nrow(cellexalObj@meta.gene))
			#browser()
			n[OK] = as.vector(lables[which(is.na(OK_genes) ==F)])
			cellexalObj@meta.gene <- cbind( cellexalObj@meta.gene, n )
			colnames(cellexalObj@meta.gene)[ncol(cellexalObj@meta.gene)] = name
			cellexalObj@usedObj$GOIs = c( cellexalObj@usedObj$GOIs, name)
			
			invisible(cellexalObj)
		} )

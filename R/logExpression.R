#' logExpression will create a section in the log document including 
#'
#' (1) the DRC the grouping was selected from (colored 2D)
#'
#' (2) the heatmap itself
#'
#' (3) a GO analysis of the genes displayed in the heatmap (using ontologyLogPage())
#' @name logExpression
#' @docType methods
#' @description Add a VR created heatmap into the log.
#' @param cellexalObj the cellexalvrR object
#' @param gene the genes displayed on the heatmap (default NULL)
#' @param drc the drc to plot the expression on
#' @param ... options you want to send to the ontologyLogPage() function
#' @title add the VR created heatmap to the log system
#' @export 
setGeneric('logExpression', ## Name
	function ( cellexalObj, gene = NULL, grouping, ... ) {
		standardGeneric('logExpression')
	}
)


#' @rdname logExpression
setMethod('logExpression', signature = c ('cellexalvrR'),
	definition = function ( cellexalObj, gene = NULL, drc, ...  ) {
	## here I need to create a page of the final log

	
	content = paste( sep="\n",
		paste( "##", " Gene expression for gene", gene  ),
		"", drcFiles2HTMLexpression( cellexalObj, drc, GOI=gene )
	)
	
	cellexalObj = storeLogContents( cellexalObj, content, type='GeneExpr')
	id = length(cellexalObj@usedObj$sessionRmdFiles)
	cellexalObj = renderFile( cellexalObj, id, type='GeneExpr' )
	
	cellexalObj
} )


#' @rdname logExpression
setMethod('logExpression', signature = c ('character'),
		definition = function (cellexalObj, genes, png, grouping, ... ) {
			cellexalObj <- loadObject(cellexalObj) #function definition in file 'lockedSave.R'
			logExpression(cellexalObj, genes, png, grouping, ... ) #function definition in file 'logExpression.R'
		}
)

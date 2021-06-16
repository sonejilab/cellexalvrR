#' Plot a simple violin plot using the cellexalVR colors
#'
#' @name plotViolin
#' @docType methods
#' @description Create a simple violin plot on the expresion of one gene
#' @param x the cellexalvrR object
#' @param gene the gene name
#' @param grouping either a column in the userGroups table or a CellexalVR selection file
#' @param ofile the figure out file (or null)
#' @param width default figure width (9 in)
#' @param height default figure height (9 in)
#' @param main the figure title (defaults to gene name)
#' @param X11type not important here (default to 'cairo')
#' @param family pdf typing family (defaults to "Helvetica") png is also supported
#' @title create violin plots for a list of genes and a grouping (interactive only)
#' @export 
setGeneric('plotViolin', ## Name
	function (x, gene, grouping, ofile=NULL, 
	width=9, height=9, main=NULL, X11type= 'cairo', family="Helvetica"  ) { ## Argumente der generischen Funktion
		standardGeneric('plotViolin') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)



#' @rdname plotViolin
setMethod('plotViolin', signature = c ('cellexalvrR'),
	definition <- function(x, gene, grouping, ofile=NULL, width=9, height=9, main=NULL, 
		X11type= 'cairo', family="Helvetica"  ) {

	x <- loadObject(x) #function definition in file 'lockedSave.R'
	x <- userGrouping(x, grouping) #function definition in file 'userGrouping.R'

	if ( is.null(main) ){
		main = gene
	}
	ok <- which(!is.na(x@userGroups[,x@usedObj$lastGroup]))
	if ( length(ok) > 0) {
		loc <- reduceTo (x, what='col', to=colnames(x@data)[ ok ] ) #function definition in file 'reduceTo.R'
	}else {
		loc <- x
	}
	nam= unique( loc@userGroups[,x@usedObj$lastGroup])
	data = lapply( nam , 
		function( name ){
			loc@data[gene, which(loc@userGroups[,x@usedObj$lastGroup] == name)]
		} )
	names(data)[0] = 'x'
	col = NULL
	for (name in nam) {
		col = c(col, x@colors[[x@usedObj$lastGroup]][name])
	}
	if ( !is.null(ofile) ) {
		if ( fileType == 'pdf'){
			grDevices::pdf( file=paste(ofile ,'pdf',sep='.'), width=width, height=height, family=family)
		}	
	}
	vioplot( data, names=nam, col=col, main=main )
	if ( !is.null(ofile) ){
		grDevices::dev.off()
	}
	message('Done')
	invisible(x)
} 
)


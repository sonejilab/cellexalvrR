#' @name show
#' @docType methods
#' @description show the objects internals
#' @param object the cellexalGrouping
#' @title show for a cellexalGrouping object
#' @export
setMethod('show', signature = c ('cellexalGrouping'),
	definition = function ( object ) {
		cat (paste("An object of class", class(object)),"with gname", object@gname,"\n" )
		cat (paste( 'with',length(table(object@grouping)),
			'groups selected from', object@drc,'drc model'),"\n")
		cat (paste( 'based on the selection file',object@selectionFile),"\n")

		if ( length( object@heatmapBasename) != 0){
			cat( paste( "The heatmap basname is",object@heatmapBasename,"\n" ))
		}
		cat('\n')
	}
)



#' @rdname HTMLtable
setMethod('HTMLtable', signature = c ('cellexalGrouping'),
	definition = function ( x ) {

		tableLine = function(id ) {
			OK = which( x@grouping == id)
			paste(sep="",
				'<tr><td style="background-color:',
				x@col[id],'"></td><td>',
				x@col[id],"</td><td>",
				length(OK),"</td><td>",
				x@VRgrouping[OK[1]], "</td><td>",
				id,"</td></tr>"
			)
		}
		if ( length( x@VRgrouping) == 0){
			x@VRgrouping = x@grouping -1
		}
		paste( sep="", collapse="",
			"\n### group information table\n\n",
			'<table>\n<tr><th>Color</th><th>HTML tag</th><th>cell count [n]</th><th>VR ID</th><th>R ID</th></tr>',
			paste(sep="\n",collapse="\n",sapply( 1:length(x@col), tableLine) ),"</table>"
		)
} )

#' @name plotGroup
#' @docType methods
#' @description plotGroup the cellexalGrouping
#' @param y the cellexalvrR object
#' @param x the cellexalGrouping
#' @param gname the gene name to plot expression for (default NULL)
#' @param showIDs show the group IDs in the plot ( default = TRUE)
#' @title show for a cellexalGrouping object
#' @export
setGeneric('plotGroup', ## Name
function ( y, x, gname=NULL, showIDs = TRUE   ) { 
	standardGeneric('plotGroup')
}
)

#' @rdname plotGroup
setMethod('plotGroup', signature = c ( 'cellexalvrR', 'cellexalGrouping'),
	definition = function ( y, x, gname=NULL, showIDs = TRUE ) {

	if ( is.null(gname)){
		x@grouping[ which(is.na(x@grouping))] = 0

		#x@grouping = as.numeric(as.factor(x@grouping))
		if ( ! x@drc %in% names(y@drc) ){
			stop( paste("group info does not match to cellexalvrR object data content: drc named", 
				x@drc, "not in list", paste( collapse=", ", names(y@drc))))
		}

		#x@usedObj$samples[,group] = factor( x@usedObj$samples[,group] )

	    #options(repr.plot.width=24, repr.plot.height=24)
	    gr = factor(x@grouping+1)

    	if ( length(y@drc[[x@drc]][,1]) != length(gr) ){
	    	OK = match( rownames(y@drc[[x@drc]]), colnames(y@data))
    		gr = gr[OK]
    	}
    }else {
    	data = NULL
		OK = match( tolower(gname), tolower(rownames(y@data)))
		OK = OK[which(!is.na(OK))]
		if ( length(OK) == 1 ){ ## one gene
			data = y@data[OK,]
		}else if ( length(gname) > 1 ){ ## mean expression
			data = as.vector(t(FastWilcoxTest::collapse( Matrix::t( y@data[OK,] ), 
				as.integer( rep(1, length(OK))), 1 )))
			gname = paste(gname[1], 'and', length(OK)-1, sep="_" )
		}else {
			stop("I need at least one gene to plot!")
		}
		brks=10
		brks <- unique(as.vector(c(0, stats::quantile(data,seq(0,1,by=1/brks)),max(data))))
		if ( brks[1] ==0 ){ 
			brks =c(-0.0001, 0.0001, brks[-1])
		}
		heapmapCols = function(x){ c("black", gplots::bluered(length(x)))}
		gr = cut(data, brks)
		gr = as.numeric(factor( gr ))
		showIDs = FALSE
    }

	toPlot = data.frame(x=y@drc[[x@drc]][,1], y=y@drc[[x@drc]][,2], id=gr )
    p= prettyPlot2D( toPlot, x@col, showIDs = showIDs )
    p
} )
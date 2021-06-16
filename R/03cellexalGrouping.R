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


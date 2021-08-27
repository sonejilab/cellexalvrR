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


#' @name exportSelection
#' @docType methods
#' @description Export this linear selection as CellexalVR selection file
#' @param x the object
#' @param fname the file to write the info to
#' @title export a cellexalLinear object for CellexalVR
#' @export 

#' @rdname exportSelection
setMethod('exportSelection', signature = c ('cellexalGrouping'),
	definition = function (x, fname) {

	dat = x@dat[order(x@dat$time),]
	d = cbind( rownames(dat), as.vector(dat$col), rep( x@drc , nrow(dat) ), as.numeric( dat$col )  )
	utils::write.table( d, col.names=F, row.names=F, quote=F, sep="\t", file= fname)

	f2 = paste( sep=".",fname,'points')

	d = cbind( names(dat$c), dat$x, dat$y, dat$z  )
	utils::write.table( d, col.names=F, row.names=F, quote=F, sep="\t", file=f2)

	invisible(x)
} )


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


#' Analyze each group in the selection by splitting it up into groups based on the other selection.
#' The samples will then be analzed using wilcox test as in the multi group comparisons.
#' @rdname compareGeneClusters
setMethod('compareGeneClusters', signature = c ('cellexalGrouping', 'cellexalGrouping', 'cellexalvrR' ),
definition = function (x, other, cellexalObj, altGroupNames=NULL, color=NULL, GOIs=NULL ) {


	if ( is.null(color) ) {
		color= grDevices::rainbow(2)
	}
	figTab = NULL
	if (  is.null(altGroupNames) ) {
		altGroupNames = c( x@gname,  other@gname)
	}
	cellexalObj@groupSelectedFrom[[other@gname]]@drc = x@drc


	text = paste( "## Detailed Cell cluster analysis\n\n",
		"Here all groups from the main clustering ",altGroupNames[1], " are subset based on the clustering ", altGroupNames[2] ,".",
		"Each comparison will add another Statistics section after this section.",
		"The following figure(s) show the distribution of the grouping", x@gname,":",
		paste(collapse = "\n", sep="\n",drcFiles2HTML(cellexalObj, x, showIDs = TRUE ))
		)

	cellexalObj = storeLogContents(cellexalObj, text,  type="GeneClusterComparison" )
  	id = length(cellexalObj@usedObj$sessionRmdFiles)
   	cellexalObj = renderFile(cellexalObj, id, type = "GeneClusterComparison")

   	genes = NULL

   	for (id in names(table(x@grouping)) ){
   		tmp = reduceTo( cellexalObj, what='col', to=colnames(cellexalObj@data)[which(x@grouping == id)] )
   		tmp = getDifferentials( tmp, other@gname, 
   			text=paste( "Group",x@gname,"cluster",id,"analyzed for differential expression of grouping",other@gname ) )
   		genes = c( genes, tmp@usedObj$deg.genes)
   	}

   	text = paste( "## Detailed Cell cluster analysis - summary\n\n",
   		paste( "Each group in the grouping", x@gname, "was split according to the grouping", other@gname,"." ),
   		paste( "And during", id, "statistical analyses the top 250 genes have been collected."),

   		paste("As the", id, "groups in grouping",x@gname,"are all split based on the second information,",
   			"genes that occure more often in this tests might indicate a general difference",
   			"between the groups in the second grouping (",other@gname,")." )
   		)

   	tab = table(genes)
   	tab2 = table(tab)

   	for (nRep in rev(names(tab2))){
   		genes = names(which(tab == nRep))
   		nGenes = length(genes)
   		text=paste(text, "\n\n",nGenes, "genes have been detected in", nRep, "statistical comparisons:\n\n",
   			md_gene_links( sort(genes) ) )
   	}

	cellexalObj = storeLogContents(cellexalObj, text,  type="GeneClusterComparison" )
  	id = length(cellexalObj@usedObj$sessionRmdFiles)
   	cellexalObj = renderFile(cellexalObj, id, type = "GeneClusterComparison")

   	cellexalObj
}

)
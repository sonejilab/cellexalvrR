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

#' The cellexalvrR sanity check.
#' Prints out the problems and sets a x@usedObj$ceckPassed boolean value.
#' This function is called in the export2cellexalvr function and others.
#'
#' @name check
#' @docType methods
#' @description checks cellexalvrR internals
#' @param x the cellexalvrR object
#' @param silent do not report warnings (default FALSE)
#' @title cellexal internal checks
#' @returns the checked cellexalvrR object
#' @export 
setGeneric('check', ## Name
	function ( x, silent=FALSE ) {
		standardGeneric('check')
	}
)

#' @rdname check 
setMethod('check', signature = c ('cellexalGrouping'),
	definition = function ( x, silent=FALSE ) {
		if (x@gname ==""){
			stop("gname not set - empty?")
		}
		#if ( ! file.exists( x@selectionFile) ){
		#	warning( paste("selection file", x@selectionFile, "does not exists") )
		#}
		if ( is.null(names(x@grouping))){
			warning("The grouping data is not named")
		}
		if ( length(x@col) == 0 ){
			warning("The color is not set - filling in rainbow")
			x@col = rainbow( length( table(x@grouping) ) ) 
		}
		invisible(x)
	}
)


#' @rdname describesTheseCells
setMethod('describesTheseCells', signature = c ('cellexalGrouping'),
definition = function ( x, cells ) {
	m = match( names(x@grouping), cells )
	good =  m[which(! is.na(m))]
	bad = m[which(is.na(m))]
	length(which( is.na( x@grouping[good] ))) == 0 && 
		length(which(! is.na( x@grouping[bad] ))) == 0
 } )


#' @rdname subset
setMethod('subset', signature = c ('cellexalGrouping'),
definition = function ( x, cells) {
	m = match( names(x@grouping), cells )
	m = m[which(is.na(m))] # we need to get rid of data!

	x@grouping[m] = NA
	x@order[m] = NA

	existing_files <- list.files(pattern = "^selection\\d+\\.txt$")
	new = NULL
	if (length(existing_files) == 0) {
		new="selection0.txt"
	}else{
		numbers <- as.integer(gsub("[^0-9]", "", existing_files))
		new=paste(sep="","selection",max(numbers) + 1,".txt")
	}
	exportSelection( x, new)
	x@selectionFile = new

	id= (ncol(cellexalObj@userGroups) /2) + 1
	x@gname = paste( "User.group", id, sep="." )

	invisible(x)
} )


#' @rdname pseudoBulk
setMethod('pseudoBulk', signature = c ('cellexalGrouping'),
	definition = function ( object, cellexalObj, minSize=10, minBulks = 10, maxBulks = 500 ) {

		tab = table( object@grouping)
		min_cells = min(tab)
		toCells = minSize
		if ( floor(min_cells/minSize) < minBulks ){
			warning( "one group has very view cells - created less pseudo samples that wanted")
			toCells = minSize
			if ( floor(min_cells/toCells) < 2 ){
				## that is really really too little cells here!
				toCells = ceiling(min_cells/2)
			}
		}else if ( floor(max(tab)/minSize) > maxBulks ){
			toCells = ceiling(max(tab)/maxBulks)
			if ( floor(min_cells/toCells) < minBulks ){
				toCells = floor(max(tab)/minBulks)
				warning( paste("using more than",maxBulks,"max pseudo samples in a group"  ) )
			}
		}

		toIDs = function(n) {
			toGroups = floor(n / toCells)
			if ( toGroups  == 0 ){
				toGroups = 2
			}
			if ( is.na(toGroups)){
				browser(text="toGroups is not defined!")
			}
			tryCatch( {
				ids = rep( 1:toGroups, floor(n/toGroups))
				},
				error = function(err){ print(err); browser( text="in the try catch construct an error has occured")})
			ids = rep( 1:toGroups, floor(n/toGroups))
			ids = c( ids, sample( 1:toGroups, n%%toGroups))
			ids = sort(ids)
			ids
		}

		cellexalObj = reorderSamples ( cellexalObj, object@gname )
		object = groupingInfo(cellexalObj, object@gname)

		ids =  toIDs(tab[1])
		for (n in tab[-1]) {
			ids = c( ids, toIDs(n) + max(ids))
		}

		# now I need to construct the pseudoBulk object...
		PseudoBulk = reduceTo( cellexalObj, what='col', to= names(object@grouping)[match(unique(ids), ids)])
		PseudoBulk@usedObj$pseudoBulk_groupings = ids
		names(PseudoBulk@usedObj$pseudoBulk_groupings) = colnames(cellexalObj@data)
		
		PseudoBulk@data = Matrix(FastWilcoxTest::collapse( cellexalObj@data, ids, type=2 ), sparse=TRUE) ## mean expression

		rownames(PseudoBulk@data) = rownames(cellexalObj@data)
		colnames(PseudoBulk@data) = names(object@grouping)[match(unique(ids), ids)]

		invisible ( PseudoBulk )
		
	}
)


#' @rdname exportSelection
setMethod('exportSelection', signature = c ('cellexalGrouping'),
	definition = function (x, fname) {

	d = cbind( names(x@grouping), x@col[as.vector(x@grouping)],
	 rep( x@drc , length(x@grouping) ), as.numeric(x@grouping)-1  )
	utils::write.table( d, col.names=F, row.names=F, quote=F, sep="\t", file= fname)

#	f2 = paste( sep=".",fname,'points')

#	d = cbind( names(dat$c), dat$x, dat$y, dat$z  )
#	utils::write.table( d, col.names=F, row.names=F, quote=F, sep="\t", file=f2)

	invisible(x)
} )


#' @rdname HTMLtable
setMethod('HTMLtable', signature = c ('cellexalGrouping'),
	definition = function ( x, addCol=NULL ) {

		x = check(x)
		if ( length( x@VRgrouping) == 0){
			x@VRgrouping = as.numeric(x@grouping) -1
		}
		tableLine = function( id ) {
			OK = which( as.numeric(x@grouping) == id)
			paste(sep="", collapse="",
				'<tr><td style="background-color:',
				x@col[id],'"></td><td>',
				x@col[id],"</td><td>",
				length(OK),"</td><td>",
				x@VRgrouping[OK[1]], "</td><td>",
				names(table(x@grouping))[id],"</td></tr>"
			)
		}

		ret = paste( sep="", collapse="",
			"\n### group information table\n\n",
			'<table>\n<tr><th>Color</th><th>HTML tag</th><th>cell count [n]</th><th>VR ID</th><th>R ID</th></tr>',
			paste(sep="",collapse="",sapply( 1:length(x@col), tableLine) ),"</table>"
		)


		if ( ! is.null(addCol) ){
			#browser(text="This table is not OK - check why!")
			tableLine = function( id ) {
			OK = which( as.numeric(x@grouping) == id)
			## the not selected cells!
			ret = NULL
			if (id == 0){
				addvalue = addCol[[1]][id]
				col = grey(.6)
			}
			else {
				id = as.numeric(id)
				addvalue = addCol[[1]][paste(id)]
				col = x@col[id]
			}
			ret = paste(sep="", collapse="",
				'<tr><td style="background-color:',
				col,'"></td><td>',
				col,"</td><td>",
				length(OK),"</td><td>",
				x@VRgrouping[OK[1]], "</td><td>",
				names(table(x@grouping))[id],"</td>", "<td>", addvalue, "</td></tr>"
			)
			return (ret)
			}
			ret = paste( sep="", collapse="",
			"\n### group information table\n\n",
			'<table>\n<tr><th>Color</th><th>HTML tag</th><th>cell count [n]</th><th>VR ID</th><th>R ID</th><th>',names(addCol)[1],'</th></tr>',
			paste(sep="",collapse="",sapply( names(table(as.numeric(x@grouping))), tableLine) ),"</table>"
			)
		}
		return (ret)
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
   		#browser(text= "check that the tmp has the correct data." )
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


#' @name writeNetworks
#' @docType methods
#' @description writeNetworks the cellexalGrouping
#' @param x the cellexalGrouping
#' @param path the path to write the files to
#' @title export the network information to a list of files
#' @export
setGeneric('writeNetworks', ## Name
function ( x, path=NULL ) { 
	standardGeneric('writeNetworks')
}
)

#' @rdname writeNetworks
setMethod('writeNetworks', signature = c ( 'cellexalGrouping'),
	definition = function ( x, path ) {
		if ( ! dir.exists( path )){
			dir.create( path )
		}
		if ( length( x@network ) == 0){
			stop("the networks have not been stored correctly!")
		}
		networkF = filename( c("Networks","nwk"), path)
		utils::write.table(x@network$"Networks",file.path( path, networkF ),row.names=F,col.names=T,quote=F,sep="\t",eol="\r\n")
		centroidsF = filename( c("NwkCentroids","cnt"), path)
        utils::write.table(x@network$"NwkCentroids",file.path( path,centroidsF),row.names=F,col.names=F,quote=F,sep="\t",eol="\r\n")
        return (c( file.path(path, networkF), file.path( path,centroidsF) ))
})

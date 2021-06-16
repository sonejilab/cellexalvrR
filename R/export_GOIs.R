#' Normally a scientis has a number of genes that are of high interest for the specific data set.
#' This function can be used to export this gene list as a buch of hetmap input files for cellexalVR.
#' 
#' These files can be loaded from the cellexalVR console by using the command
#' GOIs path
#' A specificly modified CellexalVR version is required for this.
#'
#' @name export_GOIs
#' @docType methods
#' @description export a list of genes of interest to be loaded as heatmaps in cellexalVR
#' @param x the cellexal object
#' @param GOIs the genes of interest
#' @param grouping the grouping the hetmap should be using (a cellexalVR selection file will be produced)
#' @param path the outpath for the files
#' @param colorF a function returning the colors in the order they should appear on the heatmap (default = rainbow)
#' @title export a list of genes of interest to be loaded as heatmaps in cellexalVR
#' @export 
#if ( ! isGeneric('renew') ){
setGeneric('export_GOIs', ## Name
	function (x, GOIs, grouping, path, colorF = rainbow ) { 
		standardGeneric('export_GOIs')
	}
)
#}


#' @rdname export_GOIs
setMethod('export_GOIs', signature = c ('cellexalvrR'),
	definition = function (x, GOIs, grouping, path, colorF = rainbow ) {
	if ( is.null( x@userGroups[,grouping] )){
		stop(paste("Grouping", grouping,"not in the object!") )
	}
	m = match(GOIs, rownames(x@data))
	if ( length(which(is.na(m))) > 0 ){
		stop(paste("These genes are not in the object:", paste(collapse=", ", GOIs[which(is.na(m))])))
	}
	if ( ! file.exists(path) ){
		stop(paste("Path",path, "does not exist!" ))
	}
	gene_cuts <- split(GOIs, ceiling(seq_along(GOIs)/250))
	## the drc the groups have been selected from is unknown - take the first one...
	cellidfile = file.path( path, "GOIs_selection.txt")
	# name color drcName id - no colnames!
	cols = colorF(length(table(x@userGroups[,grouping])))[x@userGroups[,grouping]]
	sel = data.frame( colnames(x@data), cols, rep(names(x@drc)[1], nrow(x@userGroups)), x@userGroups[,grouping] )
	sel = sel[order( x@userGroups[,grouping] ),]
	sel[,2] = colorF( length( levels(sel[,2]) ))[ as.numeric(factor( sel[,2], levels=unique(sel[,2]))) ]
	utils::write.table( sel, file= cellidfile, quote=FALSE, row.names=FALSE, sep="\t", col.names=FALSE)
	for ( i in 1:length(gene_cuts)) {
			outfile=file.path( path,paste(sep="", "GOIS_slice_",i,".txt") )
			x@groupSelectedFrom[[x@usedObj$lastGroup]][["heatmapBasename"]] = basename( cellidfile )
			gene.cluster.order = gene_cuts[[i]]

			message (paste( "trying to write file", outfile, "containing", length(gene.cluster.order), "genes") )
			write(c(length(gene.cluster.order),gene.cluster.order),file=outfile,ncolumns=1)
			## probably a good way to export the information as database, too.
			## we only need the GOIs
			tmp = reduceTo(x, what='row', to=  gene.cluster.order ) #function definition in file 'reduceTo.R'
			## we also only need the samples that have been selected:
			#tmp = reduceTo(tmp, what='col', to=  colnames(x@data)[ #function definition in file 'reduceTo.R'
			#				which(!is.na(x@userGroups[,x@usedObj$lastGroup])) ] )

			try ( write_as_sqlite3( tmp, paste(sep=".", outfile, 'sqlite3') ) ) #function definition in file 'ExportFunctions.R'
	}
	invisible(x)
} )

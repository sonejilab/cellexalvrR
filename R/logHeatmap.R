#' logHeatmap will create a section in the log document including 
#'
#' (1) the DRC the grouping was selected from (colored 2D)
#'
#' (2) the heatmap itself
#'
#' (3) a GO analysis of the genes displayed in the heatmap (using ontologyLogPage())
#' @name logHeatmap
#' @docType methods
#' @description Add a VR created heatmap into the log.
#' @param cellexalObj the cellexalvrR object
#' @param genes the genes displayed on the heatmap (default NULL)
#' @param png the VR generated heatmap (png)
#' @param grouping the grouping file used to create this heatmap
#' @param ... options you want to send to the ontologyLogPage() function
#' @title add the VR created heatmap to the log system
#' @export 
setGeneric('logHeatmap', ## Name
	function ( cellexalObj, genes = NULL, png, grouping, ... ) {
		standardGeneric('logHeatmap')
	}
)


#' @rdname logHeatmap
setMethod('logHeatmap', signature = c ('cellexalvrR'),
	definition = function ( cellexalObj, genes = NULL, png, grouping, ...  ) {
	## here I need to create a page of the final log

	
	if ( !is.null(genes)){
		if ( file.exists(genes[1])) {
			genes = as.vector(utils::read.delim(genes[1])[,1])
		}
	}
	if ( ! file.exists( png) ) {
		stop(paste( "logHeatmap the heatmap png file can not be found!", png ) )
	}

	# if we got a file - let's read that in!
	if ( file.exists( grouping)) {
		cellexalObj = userGrouping( cellexalObj, grouping ) #function definition in file 'userGrouping.R'
		grouping = cellexalObj@usedObj$lastGroup
	}

	## the grouping needs to be matched with the heatmap

	base = unlist(strsplit( png, '_' ))
	base = paste( paste( collapse="_",base[-length(base)] ), sep=".", 'txt')

	heatmap_core =  basename(base)
	
	ok = which( unlist( lapply( cellexalObj@groupSelectedFrom, 
           function(info){!is.na(match(info@heatmapBasename, heatmap_core )) }
    )))
    if ( length( ok ) > 0 ){
        grouping = names(rev(ok)[1])
    }

	cellexalObj = sessionPath( cellexalObj ) #function definition in file 'sessionPath.R'
	sessionPath = cellexalObj@usedObj$sessionPath

	## copy the CellexalVR files to the log folder
	

	## set the lastGroup id to grouping
	
	cellexalObj = sessionRegisterGrouping( cellexalObj, grouping ) #function definition in file 'sessionRegisterGrouping.R'
	n = sessionCounter( cellexalObj, grouping ) #function definition in file 'sessionCounter.R'
	cellexalObj@usedObj$lastGroup = grouping

	figureF = file.path( sessionPath , 'png', basename( png ) )
	file.copy(png, figureF )

	## in order to restore this heatmap in a new VR session I now need to store the database and gene list, too
	if ( ! file.exists( file.path( sessionPath , 'Heatmap') ) ) {
		dir.create( file.path( sessionPath , 'Heatmap') )
	}
	file.copy(base, file.path( sessionPath , 'Heatmap', basename( base ) ) )
	file.copy(paste(base, sep=".", 'sqlite3'), file.path( sessionPath , 'Heatmap', basename( base ) ) )

	## now I need to create the 2D drc plots for the grouping
	#cellexalObj = userGrouping(cellexalObj, grouping ) #function definition in file 'userGrouping.R'
	
	gInfo = groupingInfo( cellexalObj, cellexalObj@usedObj$lastGroup ) #function definition in file 'groupingInfo.R'

	tableHTML = HTMLtable ( gInfo) 
	if ( nrow( gInfo@timeObj@dat) > 0 ){
		tableHTML = HTMLtable ( gInfo@timeObj )
	}
	
	

	figureF = correctPath(figureF, cellexalObj)
	content = paste( sep="\n",
		paste( "##", "Heatmap from Saved Selection ", n  ),
		paste("This selection is available in the R object as group",cellexalObj@usedObj$lastGroup ),
		"",
		paste( "### Genes"),
		md_gene_links( sort(genes) ), #function definition in file 'rmdLink.R'
		md_gene_links( rev(genes), label="expand genes in heatmap order" ),
		'',
		paste( "### Heatmap (from CellexalVR)"),
		paste("![](",figureF,")"),
		'',
		tableHTML,'',
		paste(collapse = "\n", sep="\n",drcFiles2HTML(cellexalObj, gInfo )),
		"The heatmap can be restored in a new VR session using the 2D console (F12) and type:",
		"","```",
		paste("lsf", file.path( cellexalObj@usedObj$sessionPath, gInfo@selectionFile) ),
		"",
		"confirm", 
		"",
		paste( "rsf",  file.path( cellexalObj@usedObj$sessionPath, 'Heatmap',basename(base))),
		"```",
		"These files are also part of the Portable session log file. Please adjust the paths accordingly."

	)

	cellexalObj = storeLogContents( cellexalObj, content, type='Heatmap')
	id = length(cellexalObj@usedObj$sessionRmdFiles)
	cellexalObj = renderFile( cellexalObj, id, type='Heatmap' )

	if ( ! file.exists(file.path(sessionPath, '..', "cellexalObj.RData") )){
		lockedSave(cellexalObj, file.path(sessionPath, '..') ) #function definition in file 'lockedSave.R'
	}else {
		savePart(cellexalObj, 'usedObj' ) #function definition in file 'integrateParts.R'
	}
	
	cellexalObj
} )


#' @rdname logHeatmap
setMethod('logHeatmap', signature = c ('character'),
		definition = function (cellexalObj, genes, png, grouping, ... ) {
			cellexalObj <- loadObject(cellexalObj) #function definition in file 'lockedSave.R'
			logHeatmap(cellexalObj, genes, png, grouping, ... ) #function definition in file 'logHeatmap.R'
		}
)

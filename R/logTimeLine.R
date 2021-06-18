#' logTimeLine will create a section in the log document including 
#' (1) the DRC the grouping was selected from (colored 2D)
#' (2) the heatmap itself
#' (3) a GO analysis of the genes displayed in the heatmap (using ontologyLogPage()) #function definition in file 'ontologyLogPage.R'
#' @name logTimeLine
#' @rdname logTimeLine
#' @docType methods
#' @description preload the object before creating one Heatmap session report page
#' @param cellexalObj the cellexalvrR object
#' @param stats the correlation statistics
#' @param genes the genes to display on the heatmap
#' @param info the original grouping information list
#' @param png the heatmap of the rolling sum data
#' @param timeInfo the time grouping information list
#' @param GOIs an optional vector of genes to plot rolling sum graphs for.
#' @param text additional text for the HTML file (default = NULL)
#' @title add the timeline information to the log system
#' @export 
setGeneric('logTimeLine', ## Name
	function ( cellexalObj, stats, genes=NULL, info, png, timeInfo , GOIs=NULL, text=NULL ) {
		standardGeneric('logTimeLine')
	}
	)


#' @rdname logTimeLine
setMethod('logTimeLine', signature = c ('cellexalvrR'),
	definition = function ( cellexalObj, stats, genes=NULL, info, png, timeInfo, GOIs=NULL, text=NULL ) {
	## here I need to create a page of the final log

	if ( VRmode() ){
		warning("In VR mode the detailed timeline report is deactivated.")
		content = paste(collapse="\n", sep="\n","",
			"##", "TimeLine control from Saved Selection ", 
			sessionCounter( cellexalObj, cellexalObj@usedObj$lastGroup ) ,"",
			paste("This TimeLine is available in the R object as group",
				timeInfo@gname ) ,""
			)
		content =  paste( collapse="\n", sep="\n", "In VR the time consuming in detail timeline analysis is deactivated - to get the detailed analysis you need to run this script directly in R:",
			"```",
			"library(cellexalvrR)",
			"load(<the cellexalObj.RData in the Data folder>)",
			"cellexalObj = sessionPath(cellexalObj, 'MyCommandLineSession')",
			"getDifferentials( cellexalObj, <you selection file>)",
			"cellexalObj = renderReport(cellexalObj)",
			"```",
			"You obviousely need the selection file linked in the statistics section above and the cellexalObj.RData file from the Data folder."
			)		
	}
	else {
		cellexalObj = sessionPath( cellexalObj ) #function definition in file 'sessionPath.R'
		sessionPath = cellexalObj@usedObj$sessionPath
		cellexalObj = sessionRegisterGrouping( cellexalObj, cellexalObj@usedObj$lastGroup ) #function definition in file 'sessionRegisterGrouping.R'
		n = sessionCounter( cellexalObj, cellexalObj@usedObj$lastGroup ) #function definition in file 'sessionCounter.R'

		if (  class(timeInfo)[[1]] == 'cellexalGrouping'){
			if ( nrow(timeInfo@timeObj@dat) > 0 ){
				timeInfo = timeInfo@timeObj
			}
		}
		## now I need to create a heatmap myself using the genes provided

	#	message("find a usable way to get the heatmap png")
		#file.copy(png, file.path( sessionPath , 'png', basename( png ) ) )
		#figureF = file.path( 'png', basename( png ) )
		#figureF = "Missing at the moment!"

		## now I need to create the 2D drc plots for the grouping
		#drcFiles = drcPlots2Dtime( cellexalObj, info, GOIs ) #function definition in file 'drcPlot2Dtime.R'
		drcFiles2 = sapply(drcPlots2Dtime( cellexalObj, timeInfo ), correctPath, cellexalObj) #function definition in file 'drcPlot2Dtime.R'
		## but I also want to show the TIME in the drc plot - hence I need a new grouping!

		content = paste( collapse="\n", sep="\n","",
			paste( "##",timeInfo@gname, "TimeLine control"),
			"",
			paste("This TimeLine is available in the R object as group",
				timeInfo@gname ),
			""
			)
		
		if ( ! is.null(text) ){
			content = paste( content, "<p>", text, "</p>", collapse="\n", sep="\n")
		}

		
		if ( file.exists( png[1] ) ) {
			
			figureF = correctPath( png[1], cellexalObj )

			content = paste( collapse="\n", sep=" ", content,"",
				paste( "### Timeline plot showing mean expression of a set of genes (from R)"),
				"",paste("![](",figureF,")") ,"",
				"<p>In short: the genes are grouped by there expression pattern; 
				the mean expression values of all genes in a group per cell are collected; 
				the main expression trend is extrapolated using the loess R function and these smoothened values are plotted.</p>",
				""
				)
		}
		## genes should be a list
		
		content = paste( collapse="\n", 
			content, "", "### Detailed Gene Expression as heatmaps","","",
			"The scaling of the x axis is different from the previouse Timeline plots.",
			"They are scaled to the (arbitrary) pseudo time whereas here the heatmaps are showing each cell after the other.",
			"Hence in these plots each time 'slot' has the same size.","",
			"",paste("![](",timeInfo@geneClusters[[1]]$groupColors,")") ,""
			) 
		for ( i in 1:length(genes) ) {

			content = paste( collapse=" ", sep=" ",content,"\n\nGene group ",i,
				paste("\n![](",correctPath(png[i+1], cellexalObj),")\n"),
				md_gene_links ( sort(genes[[i]]) ),
				md_gene_links ( rev(genes[[i]]), label="expand in heatmap order" )
				)
		}
		content = paste( sep="\n",collapse="\n", content,
			#paste(collapse = "\n", sep="\n",drcFiles2HTML(cellexalObj, info, "original selection")), #function definition in file 'drcPlot2D.R'
			paste(collapse = "\n", sep="\n",drcFiles2HTMLtime(cellexalObj, info, "time line")) #function definition in file 'drcPlot2Dtime.R'

			)
	}

	cellexalObj = storeLogContents( cellexalObj, content, type="OneGroupTime")
	id = length(cellexalObj@usedObj$sessionRmdFiles)
	cellexalObj = renderFile( cellexalObj, id, type="OneGroupTime" )
	
	cellexalObj
	} )



#' @rdname logTimeLine
setMethod('logTimeLine', signature = c ('character'),
	definition = function (cellexalObj, stats, genes, info, png, timeInfo, GOIs=NULL, text=NULL  ) {
			cellexalObj <- loadObject(cellexalObj) #function definition in file 'lockedSave.R'
			logTimeLine(cellexalObj, stats, genes, info, png, timeInfo, GOIs=GOIs, text=text ) #function definition in file 'logTimeLine.R'
		}
		)

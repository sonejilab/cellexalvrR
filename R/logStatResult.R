#' logStatResult will create a clickable link in the log document 
#' where the user can download the stats table from.
#' @name logStatResult
#' @docType methods
#' @description Create a log file part including a stats data download link (coma separted table file)
#' @param x the cellexalvrR object
#' @param method the stats method used (no spaces please)
#' @param data the results table
#' @param col the p value column to plot the -log10 histogram
#' @title make the stats table available in the log system
#' @export
setGeneric('logStatResult', ## Name
	function ( x, method, data, col=NULL ) { ## Argumente der generischen Funktion
		standardGeneric('logStatResult') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)



#' @rdname logStatResult
setMethod('logStatResult', signature = c ('cellexalvrR'),
	definition = function ( x, method, data, col=NULL ) {
	if (! is.null( x@usedObj$sessionName)) {

		## export the data into a file and add a small download link into the report
		ofile =   paste( x@usedObj$lastGroup,method,"csv", sep="."   )
		if ( ! file.exists(file.path( x@usedObj$sessionPath,'tables') )) {
			dir.create( file.path( x@usedObj$sessionPath,'tables') )
		}

		utils::write.table( data, file= file.path( x@usedObj$sessionPath,'tables',ofile) , quote=F, sep="," )

		mainOfile = x@usedObj$sessionRmdFiles[1]

		## we need a table to describe the grouping - INCLUDING COLOR INFORMATION
		## the VR ids are R ID's -1 (!!) make that clearly visible.
		## get the VR order:
		gInfo = groupingInfo(x)
		
		tableHTML = HTMLtable(gInfo)
		gname = gInfo@gname

		if ( nrow(gInfo@linarObj@dat) > 0 ) {
			tableHTML = HTMLtable(gInfo@linarObj)
			gname = gInfo@gname
		}
		tableHTML = paste(tableHTML, "\n")
		## this need to become a relative path - relative to the final outfile

		if (length(grep('Time.group', x@usedObj$lastGroup)) > 0 ) {
			gname = x@usedObj$linearSelection[[x@usedObj$lastGroup]]@parentSelection
			sesionFile = x@usedObj$SelectionFiles[[gname]]
		}else {
			sesionFile = x@usedObj$SelectionFiles[[x@usedObj$lastGroup]]
		}
		content=paste( sep="\n", collapse = "\n",
				paste( "##", x@usedObj$lastGroup,"Statistical Result" ),
				"",
				tableHTML,
				"",
				"### Data",
				paste(sep="",  "<a href='",file.path( ".", x@usedObj$sessionName, 'tables',ofile),"' download>","This tab separated table </a>",
					" contains the statistical results for ",
				    "<a href='",file.path( ".", x@usedObj$sessionName, basename(sesionFile)),"' download>"," this selection file</a>", ".","\n"),
				paste(collapse = "\n", sep="\n",drcFiles2HTML(x, gInfo, showIDs = TRUE ))
		)
		## the 1-log10 p value histogram
		if ( ! is.null(col) ){

			ofile = file.path(x@usedObj$sessionName, 'png',paste( 'hist',x@usedObj$lastGroup,method,"png", sep="."   ) )
			grDevices::png( file= file.path( x@outpath, ofile ), width=800, height=800)
			dat = -log10(data[,col])
			graphics::hist( dat[which(!is.na(dat))], main = paste( x@usedObj$sessionName, gname )  )
			grDevices::dev.off()

			content = c( content, paste( sep="\n",
					paste("![](",ofile,")"),
					"" ))
		}
		
		x = storeLogContents( x, content, type='Stats') # in file renderFile.R
		id = length(x@usedObj$sessionRmdFiles)
		x = renderFile( x, id, type='Stats' )

	}
	invisible(x)
} )
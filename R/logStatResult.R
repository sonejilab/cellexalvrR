#' logStatResult will create a clickable link in the log document 
#' where the user can download the stats table from.
#' @name logStatResult
#' @aliases logStatResult,cellexalvrR-method
#' @rdname logStatResult-methods
#' @docType methods
#' @description add small section in the log file including a stats data download link (coma separted table file)
#' @param x the cellexalvrR object
#' @param method the stats method used (no spaces please)
#' @param data the results table
#' @param col the p value column to plot the -log10 histogram
#' @title add a table into the session log
#' @export
setGeneric('logStatResult', ## Name
	function ( x, method, data, col=NULL ) { ## Argumente der generischen Funktion
		standardGeneric('logStatResult') ## der Aufruf von standardGeneric sorgt f\xFCr das Dispatching
	}
)

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

		## this need to become a relative path - relative to the final outfile
		content=paste( sep="\n",
				paste( "##", "Statistical result from ",  x@usedObj$lastGroup ),
				"",
				#paste(sep="",  "<a href='",file.path( x@usedObj$sessionPath, 'tables',ofile),"' download>",ofile,"</a>" ),
				paste(sep="",  "<a href='",file.path( ".", x@usedObj$sessionName, 'tables',ofile),"' download>",ofile,"</a>" ),
				""
		)

		if ( ! is.null(col) ){
			ofile = file.path( 'png',paste( 'hist',x@usedObj$lastGroup,method,"png", sep="."   ) )
			grDevices::png( file= file.path( x@usedObj$sessionPath, ofile ), width=800, height=800)
			graphics::hist( -log10(data[,col]), main = paste( x@usedObj$sessionName, method )  )
			grDevices::dev.off()

			content = c( content, paste( sep="\n",
					paste("![](",ofile,")"),
					"" ))
		}
		x = storeLogContents( x, content, type='Stats')
		id = length(x@usedObj$sessionRmdFiles)
		x = renderFile( x, id, type='Stats' )

	}
	invisible(x)
} )

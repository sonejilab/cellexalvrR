#' Add the last entry into the log file
#' A short explanation and a like to the folder with the ShinyCell data
#'
#' @name logStatResult
#' @docType methods
#' @description Create a log file part including a stats data download link (coma separted table file)
#' @param x the cellexalvrR object
#' @param text additional text for manual logs
#' @param path and optional path to create the shiny app in (defaults to the session path)
#' @title make the stats table available in the log system
#' @export
setGeneric('logShinyApp', ## Name
	function ( x, text='' , path=NULL) { ## Argumente der generischen Funktion
		standardGeneric('logShinyApp') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)



#' @rdname logShinyApp
setMethod('logShinyApp', signature = c ('cellexalvrR'),
	definition = function ( x, text='', path=NULL ) {
		if ( is.null(Sys.getenv("TEST_MODE")) || Sys.getenv("TEST_MODE") != "TRUE") {

			if (is.null(path)){
				path = x@usedObj$sessionPath
			}
			as_ShinyCell( x , path )

			logText( x, paste(sep="\n", 
				"<h1>ShinyCell object</h1>\n",
				text,
				"<p>A <a href='https://github.com/SGDDNB/ShinyCell'>ShinyCell</a> folder has been created in the session path. ",
				paste( sep="", 
					"You need RStudio and ShinyCell to be installed on your system in order to use ",
					"<a href='./",x@usedObj$sessionName,"/shinyApp/'>",
					"this folder</a>. Open the ui.app file in RStudio."
				),
				" You will then see the 'Run App' button - press it.",
				" Open this app in the browser - you then can use your VR grouings and analyze more genes.</p>")
				
			 )
		}
	}
)

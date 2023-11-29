#' prepare the cellexalObj to be used for a ShinyCell session
#' This functionality is meant to increase the usability of the log system.
#'
#' @description prepare the cellexalObj to be used for a ShinyCell session
#' @param x a cellexalvrR object 
#' @param path the path for the shinycell data
#' @title create a Shiny server.R that can be used by RStudio to build a ShinyCell session
#' @returns a Seurat object with the data from the cellexal object.
#' @export 
setGeneric('as_ShinyCell', ## Name
	function ( x , path) { 
		standardGeneric('as_ShinyCell')
	}
)


#' @rdname as_ShinyCell
setMethod('as_ShinyCell', signature = c ('cellexalvrR'),
	definition = function ( x, path ){
		if ( is.null( path) ){
			path=x@usedObj$sessionPath
		}
		if ( is.null(Sys.getenv("TEST_MODE")) || Sys.getenv("TEST_MODE") != "TRUE") {
		tryCatch(
		{
			seu = as_Seurat( x )
			#seu = ScaleData( seu )
			if (!require("ShinyCell", quietly = TRUE ) == T ) {
				stop("package 'ShinyCell' needed for this function to work. Please install it.",
						call. = FALSE)
			}
			require("Seurat", quietly = TRUE )
			scConf = createConfig(seu)

			makeShinyApp(seu, scConf, gene.mapping = TRUE,shiny.dir = file.path(path, 'shinyApp'),
				shiny.title = paste('CellexalVR log system -',x@usedObj$sessionName,'- ShinyCell'),

			)
		}
		)
		}else {
			warning("as_ShinyCell detected TEST_MODE - no action run" )
		}
	}
)
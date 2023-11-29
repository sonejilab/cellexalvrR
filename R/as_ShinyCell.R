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
		seu = as_Seurat( x )
		seu = ScaleData( seu )
		shiny_script = paste( sep="\n" ,
			"library(Seurat)",
			"library(ShinyCell)\n",
			"load('Seurat.RData')",
			"scConf = createConfig(seu)",
			"makeShinyApp(seu, scConf, gene.mapping = TRUE,",
			"             shiny.title = 'CellexalVR log system - ShinyCell')"
		)
		# Writing the script to a file
        script_file <- file.path(path, "server.R")
        writeLines(shiny_script, script_file)
        save( seu, file = file.path(path, "Seurat.RData"))
	}
)
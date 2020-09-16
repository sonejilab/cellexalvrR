setGeneric("logFigure", function(cellexalObj, genes = NULL, png, grouping, ...) {
    standardGeneric("logFigure")
})

#' logFigure is a VR helper funtion that stores one network in the log document.
#' @name logFigure
#' @aliases logFigure,cellexalvrR-method
#' @rdname logFigure-methods
#' @docType methods
#' @description Log a figure from VR. This function is used to store new screenshots from VR.
#' @param cellexalObj the cellexalvrR object
#' @param png the VR generated network (png)
#' @param text add a simmple text (default NULL)
#' @param ... options you want to send to the ontologyLogPage() function #function definition in file 'ontologyLogPage.R'
#' @title description of function logFigure
#' @export
setMethod("logFigure", signature = c("cellexalvrR"), 
    definition = function(cellexalObj, png,  text = NULL, ...) {
    	
    	cellexalObj = sessionPath(cellexalObj)  #function definition in file 'sessionPath.R'
   		sessionPath = cellexalObj@usedObj$sessionPath
   		if (!file.exists(png)) {
     	   stop(paste("logFigure - the png file can not be found: '", png,"'"))
   		}

   		file.copy(png, file.path(sessionPath, "png", basename(png)))
   		figureF = file.path("png", basename(png))

   		content = paste(
   			paste("##", "Saved figure from VR",format(Sys.time(), "%a %b %d %X %Y")),
       		paste("![](", figureF, ")"), "", paste(text),
         	"", sep = "\n"
         )
   		cellexalObj = storeLogContents(cellexalObj, content, type = "Figure")
    	id = length(cellexalObj@usedObj$sessionRmdFiles)
    	cellexalObj = renderFile(cellexalObj, id, type = "Figure")

    	invisible(cellexalObj)
    }
)
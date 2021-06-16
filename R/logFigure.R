
#' logFigure is a VR helper funtion that stores one figure in the log document.
#' 
#' @name logFigure
#' @docType methods
#' @description Log a figure from VR. This function is used to store new screenshots from VR.
#' @param cellexalObj the cellexalvrR object
#' @param png any png figure file
#' @param text add a simmple text (default NULL)
#' @title add one figure file to the log structure
#' @export
setGeneric("logFigure", function(cellexalObj, png, text=NULL) {
    standardGeneric("logFigure")
})


#' @rdname logFigure
setMethod("logFigure", signature = c("cellexalvrR"), 
    definition = function(cellexalObj, png,  text = NULL ) {
    	
    	cellexalObj = sessionPath(cellexalObj)  #function definition in file 'sessionPath.R'
   		sessionPath = cellexalObj@usedObj$sessionPath
   		if (!file.exists(png)) {
     	   stop(paste("logFigure - the png file can not be found: '", png,"'"))
   		}
      if ( ! file.exists(sessionPath )){
        stop( paste( "the cellexalvrR session path is not accessable!:", sessionPath))
      }

      figureF = file.path(sessionPath, "png", basename(png))
   		file.copy( png, figureF )
        
   		content = paste(
   			paste("##", "Saved figure from VR",format(Sys.time(), "%a %b %d %X %Y")),
       		paste("![](", correctPath(figureF, cellexalObj), ")"), "", paste(text),
         	"", sep = "\n"
         )
   		cellexalObj = storeLogContents(cellexalObj, content, type = "Figure")
    	id = length(cellexalObj@usedObj$sessionRmdFiles)
    	cellexalObj = renderFile(cellexalObj, id, type = "Figure")

    	invisible(cellexalObj)
    }
)

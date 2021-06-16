
#' logNetwork is a VR helper funtion that stores one network plot into the log document.
#' @name logNetwork
#' @docType methods
#' @description create one Network page in the session report
#' @param cellexalObj the cellexalvrR object
#' @param genes the genes displayed on the network
#' @param png the VR generated network (png)
#' @param grouping the grouping file used to create this network
#' @param ... options you want to send to the ontologyLogPage() function
#' @title add one network to the cellexalvrR log system
#' @export
setGeneric("logNetwork", function(cellexalObj, genes = NULL, png, grouping, ...) {
    standardGeneric("logNetwork")
})


#' @rdname logNetwork
setMethod("logNetwork", signature = c("cellexalvrR"), 
    definition = function(cellexalObj, genes = NULL, png, grouping, ...) {
    ## almost the same page as in the logHeatmap function - including a GO analyis?

    ## now I need to create the 2D drc plots for the grouping
    cellexalObj = userGrouping(cellexalObj, grouping)  #function definition in file 'userGrouping.R'

    cellexalObj = sessionPath(cellexalObj)  #function definition in file 'sessionPath.R'
    sessionPath = cellexalObj@usedObj$sessionPath

    if (!file.exists(png)) {
        stop(paste("logNetwork the network png file can not be found!", "png"))
    }
    figureF= file.path(sessionPath, "png", basename(png))
    file.copy(png, figureF)

    figureF = correctPath (figureF, cellexalObj )
    ## now I need to create the 2D drc plots for the grouping
    gInfo = groupingInfo(cellexalObj, cellexalObj@usedObj$lastGroup)  #function definition in file 'groupingInfo.R'

    ## gInfo is a list with names grouping, drc, col and order create a file
    ## containing the grouping info (and thereby color) and the drc info - do not
    ## create doubles

    # figureF, drcFiles[1] and drcFiles[2] do now need to be integrated into a Rmd
    # file mainOfile = file.path(sessionPath, filename( c( n, 'Network.Rmd') ) )
    # #function definition in file 'filename.R' file.create(mainOfile)
    # fileConn<-file( mainOfile )

    cellexalObj = sessionRegisterGrouping(cellexalObj, cellexalObj@usedObj$lastGroup)  #function definition in file 'sessionRegisterGrouping.R'

    content = paste(paste("##", "Network from Saved Selection", sessionCounter(cellexalObj,
        cellexalObj@usedObj$lastGroup)), paste("This selection is available in the R object as group",
        cellexalObj@usedObj$lastGroup), "", paste("### Network map (from CellexalVR)"),
        paste("![](", figureF, ")"), "", drcFiles2HTML(cellexalObj, gInfo ), sep = "\n")

    cellexalObj = storeLogContents(cellexalObj, content, type = "Network")
    id = length(cellexalObj@usedObj$sessionRmdFiles)
    cellexalObj = renderFile(cellexalObj, id, type = "Network")

    ## if you give me a gene list here you will get a GO analysis ;-) if ( !
    ## is.null(genes)){ if ( file.exists(genes)) { genes =
    ## as.vector(utils::read.delim(genes)[,1]) } cellexalObj =
    ## ontologyLogPage(cellexalObj, genes, ... ) #function definition in file
    ## 'ontologyLogPage.R' }

    invisible(cellexalObj)
})



#' @rdname logNetwork
setMethod("logNetwork", signature = c("character"), definition = function(cellexalObj,
    genes = NULL, png, grouping, ...) {
    cellexalObj <- loadObject(cellexalObj)
    logNetwork(cellexalObj, genes, png, grouping, ...)  #function definition in file 'logNetwork.R'
})

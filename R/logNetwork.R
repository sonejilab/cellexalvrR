setGeneric("logNetwork", function(cellexalObj, genes = NULL, png, grouping, ...) {
    standardGeneric("logNetwork")
})

#' logNetwork is a VR helper funtion that stores one network in the log document.
#' @name logNetwork
#' @aliases logNetwork,cellexalvrR-method
#' @rdname logNetwork-methods
#' @docType methods
#' @description create one Network page in the session report
#' @param cellexalObj the cellexalvrR object
#' @param genes the genes displayed on the network
#' @param png the VR generated network (png)
#' @param grouping the grouping file used to create this network
#' @param ... options you want to send to the ontologyLogPage() function #function definition in file 'ontologyLogPage.R'
#' @title description of function logNetwork
#' @export
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
    file.copy(png, file.path(sessionPath, "png", basename(png)))
    figureF = file.path("png", basename(png))

    ## now I need to create the 2D drc plots for the grouping
    gInfo = groupingInfo(cellexalObj, cellexalObj@usedObj$lastGroup)  #function definition in file 'groupingInfo.R'

    ## gInfo is a list with names grouping, drc, col and order create a file
    ## containing the grouping info (and thereby color) and the drc info - do not
    ## create doubles

    drcFiles = drcPlots2D(cellexalObj, gInfo)  #function definition in file 'drcPlot2D.R'

    # figureF, drcFiles[1] and drcFiles[2] do now need to be integrated into a Rmd
    # file mainOfile = file.path(sessionPath, filename( c( n, 'Network.Rmd') ) )
    # #function definition in file 'filename.R' file.create(mainOfile)
    # fileConn<-file( mainOfile )

    cellexalObj = sessionRegisterGrouping(cellexalObj, cellexalObj@usedObj$lastGroup)  #function definition in file 'sessionRegisterGrouping.R'

    content = paste(paste("##", "Network from Saved Selection", sessionCounter(cellexalObj,
        cellexalObj@usedObj$lastGroup)), paste("This selection is available in the R object as group",
        cellexalObj@usedObj$lastGroup), "", paste("### Network map (from CellexalVR)"),
        paste("![](", figureF, ")"), "", paste("### 2D DRC", gInfo$drc, " dim 1,2"),
        paste("![](", drcFiles[1], ")"), "", paste("### 2D DRC", gInfo$drc, " dim 2,3"),
        paste("![](", drcFiles[2], ")"), "", sep = "\n")

    if ( ! is.null(genes) ){
        p = as.matrix(Matrix::t(cellexalObj@data[genes, which(!is.na(cellexalObj@userGroups[,
            gInfo$gname]))]))
        ret = simplePlotHeatmaps(mat = p, fname = file.path(cellexalObj@usedObj$sessionPath,
         "png", gInfo$gname))
    
    ## the first is the summary! content =paste( content, '# Gene expression
    ## details','', '## Summary', paste( '![](',file.path('png',
    ## basename(ret$ofile)),')'), paste( collapse=' ', unlist( lapply(sort(
    ## unlist(ret$genes)), function(n) { rmdLink(n,
    ## 'https://www.genecards.org/cgi-bin/carddisp.pl?gene=') })) ), sep='\n')

        for (i in 1:length(ret$genes)) {
         content = paste(content, paste("## cluster", (i)), paste("![](", file.path("png",
             basename(ret$pngs[i])), ")"), paste(collapse = " ", unlist(lapply(sort(ret$genes[[i]]),
             function(n) {
                 rmdLink(n, "https://www.genecards.org/cgi-bin/carddisp.pl?gene=")
             }))), sep = "\n")
        }
    }
    # close(fileConn)

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


#' @describeIn logNetwork cellexalvrR
#' @docType methods
#' @description preload the cellexalObj.RData file
#' @param cellexalObj the cellexalObj.RData file
#' @param genes the genes displayed on the network
#' @param png the VR generated network (png)
#' @param grouping the grouping file used to create this network
#' @param ... options you want to send to the ontologyLogPage() function #function definition in file 'ontologyLogPage.R'
#' @title description of function logNetwork
#' @export
setMethod("logNetwork", signature = c("character"), definition = function(cellexalObj,
    genes = NULL, png, grouping, ...) {
    cellexalObj <- loadObject(cellexalObj)
    logNetwork(cellexalObj, genes, png, grouping, ...)  #function definition in file 'logNetwork.R'
})

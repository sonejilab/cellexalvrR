
#' logNetworkStats is a VR helper funtion that stores one network plot into the log document.
#' @name logNetworkStats
#' @docType methods
#' @description create one Network page in the session report
#' @param cellexalObj the cellexalvrR object
#' @param genes the genes displayed on the network
#' @param the VR generated network ()
#' @param grouping the grouping file used to create this network
#' @param ... options you want to send to the ontologyLogPage() function
#' @title add one network to the cellexalvrR log system
#' @export
setGeneric("logNetworkStats", function(cellexalObj, genes = NULL,  grouping, ...) {
    standardGeneric("logNetworkStats")
})


#' @rdname logNetworkStats
setMethod("logNetworkStats", signature = c("cellexalvrR"), 
    definition = function(cellexalObj, genes = NULL,  grouping, ...) {
    ## almost the same page as in the logHeatmap function - including a GO analyis?

    ## now I need to create the 2D drc plots for the grouping
    cellexalObj = userGrouping(cellexalObj, grouping)  #function definition in file 'userGrouping.R'

    cellexalObj = sessionPath(cellexalObj)  #function definition in file 'sessionPath.R'
    sessionPath = cellexalObj@usedObj$sessionPath


    gInfo = groupingInfo(cellexalObj, cellexalObj@usedObj$lastGroup)  #function definition in file 'groupingInfo.R'

    ## gInfo is a list with names grouping, drc, col and order create a file
    ## containing the grouping info (and thereby color) and the drc info - do not
    ## create doubles

    # figureF, drcFiles[1] and drcFiles[2] do now need to be integrated into a Rmd
    # file mainOfile = file.path(sessionPath, filename( c( n, 'Network.Rmd') ) )
    # #function definition in file 'filename.R' file.create(mainOfile)
    # fileConn<-file( mainOfile )

    cellexalObj = sessionRegisterGrouping(cellexalObj, cellexalObj@usedObj$lastGroup)  #function definition in file 'sessionRegisterGrouping.R'


    Add = as.vector( sapply(LETTERS, function(x) paste0(x, LETTERS)))[
        length( list.files( cellexalObj@usedObj$sessionPath, pattern="*Networks.nwk") ) +1
    ]
    file.copy( 
         file.path( cellexalObj@outpath,"Networks.nwk"), 
           file.path( cellexalObj@usedObj$sessionPath, paste(sep="", Add,"Networks.nwk"))
    )
    file.copy( 
         file.path( cellexalObj@outpath,"NwkCentroids.cnt"), 
         file.path( cellexalObj@usedObj$sessionPath, paste(sep="", Add,"NwkCentroids.cnt"))
    )
    #info = groupingInfo( cellexalObj, cellexalObj@usedObj$lastGroup)

    figures = paste( collapse="\n", drcFiles2HTML(cellexalObj, gInfo ))

    content = paste(sep="",

        "\n## ",gInfo@gname," Network calculation\n",
    
        paste(sep="",
        "\nA new transcription factor network has been calculated for",
        " CellexalVR based on the grouping ", gInfo@gname,
        " and this <a href='./",
        file.path(cellexalObj@usedObj$sessionName, gInfo@selectionFile),
        "' download>selection file</a>.\n"),

        "You can download the network table <a href='./",
        file.path(cellexalObj@usedObj$sessionName, paste(sep="", Add,"Networks.nwk")),
        "' download>here</a>.\n\n",

        paste( "## The 2D representation(s) of grouping",gInfo@gname,
        ":\n\nThe grouping originates from the grouping file",
        gInfo@selectionFile, "\n\n", figures )

    )

    cellexalObj = storeLogContents( cellexalObj, content, type="Networks" )
    id = length(cellexalObj@usedObj$sessionRmdFiles)
    cellexalObj = renderFile( cellexalObj, id, type='Networks' )

    ## if you give me a gene list here you will get a GO analysis ;-) if ( !
    ## is.null(genes)){ if ( file.exists(genes)) { genes =
    ## as.vector(utils::read.delim(genes)[,1]) } cellexalObj =
    ## ontologyLogPage(cellexalObj, genes, ... ) #function definition in file
    ## 'ontologyLogPage.R' }

    invisible(cellexalObj)
})



#' @rdname logNetworkStats
setMethod("logNetworkStats", signature = c("character"), definition = function(cellexalObj,
    genes = NULL,  grouping, ...) {
    cellexalObj <- loadObject(cellexalObj)
    logNetworkStats(cellexalObj, genes=genes,  grouping=grouping, ...)  #function definition in file 'logNetworkStats.R'
})

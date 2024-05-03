
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


    
    files = writeNetworks ( gInfo, cellexalObj@usedObj$sessionPath)

    #info = groupingInfo( cellexalObj, cellexalObj@usedObj$lastGroup)

    figures = paste( collapse="\n", drcFiles2HTML(cellexalObj, gInfo ))


    ## to make this data usable I need to plot the tables into single clusters.
    ## Otherwise it is quite complictaed t use this data in e.g. cytoscape.
    ## the networks file should be here:
    if ( ! file.exists(files[1]) ){
        message("The Networks.nwk file is missing!")
    }
    ## I need a file per cluster
    #dat = read.delim( file.path( cellexalObj@usedObj$sessionPath, paste(sep="", Add,"Networks.nwk")) )
    dat = gInfo@network$"Networks"
    res = lapply ( unique(dat[,7]), function(id) { dat[which(dat[,7] == id),] })
    names(res) = unique(dat[,7])

    for ( i in 1:length(res)) {
        fn = stringr::str_replace( files[1], paste(cellexalObj@usedObj$sessionPath,'/',sep=""), '')
        fn = unlist(stringr::str_split(fn,'\\.'))
        fn = filename( basename(fn), cellexalObj@usedObj$sessionPath)
        fname = file.path( cellexalObj@usedObj$sessionPath, fn)
        write.table( res[[i]], file= fname, sep="\t", quote=F, row.names=F)
        res[[i]] = paste(sep="", "<a href='./", file.path( cellexalObj@usedObj$sessionName,  fn ),
            "' download>Network File</a>")
    }
    res = list('download network' = as.vector(unlist(res)))
    names(res[[1]]) = 1:length(res[[1]])


    content = paste(sep="",

        "\n## ",gInfo@gname," Network calculation\n",
    

        paste(sep="",
        "\nA new transcription factor network has been calculated for",
        " CellexalVR based on the grouping ", gInfo@gname,
        " and this <a href='./",
        file.path(cellexalObj@usedObj$sessionName, gInfo@selectionFile),
        "' download>selection file</a>.\n"),
        "You can download the network table <a href='./", files[1], "' download>here</a>.\n\n",
        HTMLtable( gInfo, res ),"\n\n",
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

#' drcPlots2D is a function linked to the log functionallity.
#' This function will create the 2D DRC images for the log sections.
#' 
#' @name drcPlots2D
#' @docType methods
#' @description create one (2D) or three 2D drc plots for the report
#' @param cellexalObj the cellexal object
#' @param gInfo the grouping name or the cellexalGrouping object
#' @param showIDs plot the group IDs on the figure (default= TRUE)
#' @title create one (2D) or three 2D drc plots for the report
#' @export 
setGeneric('drcPlots2D', ## Name
	function ( cellexalObj, gInfo,  showIDs = TRUE ) { 
		standardGeneric('drcPlots2D')
	}
)



#' @rdname drcPlots2D
setMethod('drcPlots2D', signature = c ('cellexalvrR', 'character'),
	definition = function ( cellexalObj, gInfo,  showIDs = TRUE ) {
		gInfo = groupingInfo( cellexalObj, gInfo )
		drcPlots2D( cellexalObj, gInfo,  showIDs = showIDs )
})



#' @rdname drcPlots2D
setMethod('drcPlots2D', signature = c ('cellexalvrR', 'cellexalGrouping'),
	definition = function ( cellexalObj, gInfo,  showIDs = TRUE ) {

		cellexalObj = sessionPath(cellexalObj) #function definition in file 'sessionPath.R'
		sessionPath= cellexalObj@usedObj$sessionPath

		#print ( paste( cellexalObj@outpath, sessionPath))
		if ( ! file.exists(file.path( sessionPath , 'png') )){
			dir.create(file.path( sessionPath , 'png')  )
		}
		# if ( gInfo@gname == 'Time.group.3') {
		# 	browser()
		# }

	DRC1 = file.path( sessionPath , 'png', filename( c( gInfo@gname ,gInfo@drc , "1_2", 'png' ) )) #function definition in file 'filename.R'
	if ( ! showIDs ) {
		DRC1 = file.path( sessionPath , 'png', filename( c( gInfo@gname ,gInfo@drc , "1_2",'NoIDs', 'png' ) ))
	}

	gInfo@grouping = as.numeric( gInfo@grouping )

	## there is a possibilty that the cells have been selected from different drc models.
	## This is only recoverable if there is a drc that contains all cells.
	if ( length( gInfo@drc) > 1 ){
		all = colnames(cellexalObj@data)[which(!is.na(gInfo@grouping))]
		OK = sapply ( cellexalObj@drc, function(drc){ length(which(! is.na( match( all, rownames(drc))))) } )
		gInfo@drc = names(OK) [ which( OK  == max(OK))]
	} 

	gInfo@grouping[ which(is.na(gInfo@grouping))] = 0

	#gInfo@grouping = as.numeric(as.factor(gInfo@grouping))
	if ( ! gInfo@drc %in% names(cellexalObj@drc) ){
		stop( paste("group info does not match to cellexalObj data content: drc named", gInfo@drc, "not in list", paste( collapse=", ", names(cellexalObj@drc))))
	}

	#x@usedObj$samples[,group] = factor( x@usedObj$samples[,group] )

    #options(repr.plot.width=24, repr.plot.height=24)
    gr = factor(gInfo@grouping+1)

    if ( length(cellexalObj@drc[[gInfo@drc]][,1]) != length(gr) ){
    	OK = match( rownames(cellexalObj@drc[[gInfo@drc]]), colnames(cellexalObj@data))
    	gr = gr[OK]
    }

	grDevices::png( file= DRC1, width=1000, height=1000)

	toPlot = data.frame(x=cellexalObj@drc[[gInfo@drc]][,1], y=cellexalObj@drc[[gInfo@drc]][,2], id=gr )
    p= prettyPlot2D( toPlot, gInfo@col, showIDs = showIDs )
	print(p)
	
	grDevices::dev.off()
	DRC2 = DRC3= NULL

	if (  var( cellexalObj@drc[[gInfo@drc]][,3]) != 0 ) {
		DRC2 = file.path( sessionPath , 'png', filename(c(  gInfo@gname ,gInfo@drc, "2_3", 'png' ) )) #function definition in file 'filename.R'
		if ( ! showIDs ) {
			DRC2 = file.path( sessionPath , 'png', filename( c( gInfo@gname ,gInfo@drc , "2_3",'NoIDs', 'png' ) ))
		}

		grDevices::png( file= DRC2, width=1000, height=1000)
	
    	toPlot = data.frame(x=cellexalObj@drc[[gInfo@drc]][,2], y=cellexalObj@drc[[gInfo@drc]][,3], id=gr )
    	p= prettyPlot2D( toPlot, gInfo@col, showIDs = showIDs  ) #function definition in file drcPlot2D.R
    	print(p) ## write the plot
		grDevices::dev.off()

		DRC3 = file.path( sessionPath , 'png', filename(c(  gInfo@gname ,gInfo@drc, "1_3", 'png' ) )) #function definition in file 'filename.R'
		if ( ! showIDs ) {
			DRC3 = file.path( sessionPath , 'png', filename( c( gInfo@gname ,gInfo@drc , "1_3",'NoIDs', 'png' ) ))
		}

		grDevices::png( file= DRC3, width=1000, height=1000)
	
    	toPlot = data.frame(x=cellexalObj@drc[[gInfo@drc]][,1], y=cellexalObj@drc[[gInfo@drc]][,3], id=gr )
    	p= prettyPlot2D( toPlot, gInfo@col, showIDs = showIDs  ) #function definition in file drcPlot2D.R
    	print(p) ## write the plot
		grDevices::dev.off()

	}

	c( DRC1, DRC2, DRC3)
} )


prettyPlot2D = function(x, col, showIDs = TRUE){

	x$id = as.vector(x$id)
	x[,1] = as.numeric(x[,1])
	x[,2] = as.numeric(x[,2])
	x$col=  c(grey(.6),col)[as.numeric(x$id)]
	
	
	p = ggplot2::ggplot(x, ggplot2::aes(x=x, y=y) ) + ggplot2::theme_classic()
	p = p +   ggplot2::geom_point(color = x$col , show.legend = FALSE)

	if ( showIDs ){
	pos= t(sapply( sort(as.numeric(unique(x$id))), function(id) {
		ok = which(x$id == id); 
		c( stats::median(x[ok,1]), stats::median(x[ok,2]) )
	} ))
    theta <- seq(pi/8, 2*pi, length.out=48)
    xo <- diff(range(pos[,1]))/1200
    yo <- diff(range(pos[,2]))/1200
    C1 = c(grey(.6),col)
    C2 = 'black'
    for(i in theta) {
        p <- p + ggplot2::geom_text( data=data.frame(pos),
            ggplot2::aes_q(
                x = bquote(pos[,1]+.(cos(i)*xo)),
                y = bquote(pos[,2]+.(sin(i)*yo)),
                label=sort(as.numeric(unique(x$id)))-1), 
                    size=10, colour=C2 )
    }
    if ( length(C1) -1 == length(unique(x$id) ) ){
    	C1 = C1[-1]
    }
    p = p + ggplot2::annotate('text', x = pos[,1], y = pos[,2],
     label = sort(as.numeric(unique(x$id))) -1, size = 10, col=C1[sort(as.numeric(unique(x$id)))] )  
	}
    p
}  

#' Correct a file path so that it will be relative to the cellexalvrR@outpath
#' 
#' @name correctPath
#' @docType methods
#' @description convert the drcPlots2D file position to match the log file's position
#' @param f the file path to modify
#' @param cellexalObj the cellexal object
#' @title convert the drcPlots2D file position to match the log file's position
correctPath = function( f, cellexalObj ) {
	file.path(cellexalObj@usedObj$sessionName, 'png', basename(f)) 
}


#' drcPlots2D is a function linked to the log functionallity.
#' This function converts the file paths to Rmd image strings.
#' 
#' @name drcFiles2HTML
#' @docType methods
#' @description convert the drcPlots2D into rmd format for the log
#' @param cellexalObj the cellexal object
#' @param gInfo the return value from cellexalvrR::groupingInfo()
#' @param showIDs show the IDs on the plot (default = TRUE)
#' @param addOn a text to add in the figure heading (default NULL)
#' @title convert the drcPlots2D into rmd format for the log
#' @export 
drcFiles2HTML = function( cellexalObj, gInfo, showIDs=TRUE, addOn = NULL ) {
	## gInfo is a list with names grouping, drc, col and order
	# create a file containing the grouping info (and thereby color) and the drc info - do not create doubles
	if ( nrow(gInfo@linarObj@dat) > 0 ) {
		return( drcFiles2HTMLlinear( cellexalObj, gInfo@linarObj, showIDs= showIDs, addOn =addOn ) )
	}	
	drcFiles =sapply( drcPlots2D( cellexalObj, gInfo, showIDs=showIDs ), correctPath, cellexalObj )
	str = c(
		paste( "### 2D DRC", gInfo@drc, "dim 1,2","(", gInfo@gname,")", addOn),"\n",
		paste("![](",drcFiles[1],")"),
		'')
	if ( ! is.na(drcFiles[2]) ){
		str = c( str, 
		paste( "### 2D DRC", gInfo@drc, "dim 2,3","(", gInfo@gname,")", addOn),"\n",
		paste("![](",drcFiles[2],")"),
		"")
	}
	if ( ! is.na(drcFiles[3]) ){
		str = c( str, 
		paste( "### 2D DRC", gInfo@drc, "dim 1,3","(", gInfo@gname,")", addOn),"\n",
		paste("![](",drcFiles[3],")"),
		"")
	}
	str
}
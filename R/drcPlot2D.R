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

	DRC1 = file.path( sessionPath , 'png', filename( c( gInfo@gname ,gInfo@drc , "1_2", 'png' ), file.path( sessionPath , 'png') )) #function definition in file 'filename.R'
	if ( ! showIDs ) {
		DRC1 = file.path( sessionPath , 'png', filename( c( gInfo@gname ,gInfo@drc , "1_2",'NoIDs', 'png' ), file.path( sessionPath , 'png') ))
	}

	na = names( gInfo@grouping )
	names(gInfo@grouping) = na
    if ( length( which(is.na(gInfo@grouping)) ) > 0){
         tmp = 	as.vector(gInfo@grouping)
         tmp [which(is.na(tmp))] = 0
         gInfo@grouping = factor( tmp )
    }
    names(gInfo@grouping) = na

	#gInfo@grouping = as.numeric(as.factor(gInfo@grouping))
	if ( ! gInfo@drc[1] %in% names(cellexalObj@drc) ){
		## there is the possibility that they selected on three index values!
		## lets try this:
		indexNames = unlist(stringr::str_split( gInfo@drc[1], " - "))[1:3]
		if ( length( which(is.na( match( indexNames, colnames(cellexalObj@index))))) == 0 ){
			cellexalObj@drc[[ gInfo@drc[1] ]] = cellexalObj@index[, indexNames]
		}
		else {
			stop( paste("group info does not match to cellexalObj data content: drc named", gInfo@drc[1], 
				"not in list", paste( collapse=", ", names(cellexalObj@drc))))
		}
	}

	#x@usedObj$samples[,group] = factor( x@usedObj$samples[,group] )

    #options(repr.plot.width=24, repr.plot.height=24)
 	gr = gInfo@grouping
    if ( length(which(is.na(gInfo@grouping))) == length(which(is.na(as.numeric(as.vector(gInfo@grouping))))) ){
    	if ( min(as.numeric(as.vector(gInfo@grouping)), na.rm=T) == 0){
    		gr = factor(as.numeric(as.vector(gInfo@grouping)))
    		names(gr) = na
    	}
    }
    

    if ( length(cellexalObj@drc[[gInfo@drc[1]]][,1]) != length(gr) ){
    	OK = match( names(gr), colnames(cellexalObj@data))
    	gr = gr[OK[which(!is.na(OK))]]
    }


    ## now we can have drc's that do not contain all data!
    OK = match(rownames(cellexalObj@drc[[gInfo@drc[1]]]), names(gr))
    gr = gr[OK]
	grDevices::png( file= DRC1, width=1000, height=1000)
	toPlot = data.frame(
		x=cellexalObj@drc[[gInfo@drc[1]]][which(!is.na(OK)),1], 
		y=cellexalObj@drc[[gInfo@drc[1]]][which(!is.na(OK)),2], 
		id=gr
	)

    p= prettyPlot2D( toPlot, gInfo@col, showIDs = showIDs )
	suppressWarnings(print(p))
	grDevices::dev.off()

	DRC2 = DRC3= NULL

	if (  var( cellexalObj@drc[[gInfo@drc[1]]][,3]) != 0 ) {
		DRC2 = file.path( sessionPath , 'png', filename(c(  gInfo@gname ,gInfo@drc[1], "2_3", 'png' ), file.path( sessionPath , 'png') )) #function definition in file 'filename.R'
		if ( ! showIDs ) {
			DRC2 = file.path( sessionPath , 'png', filename( c( gInfo@gname ,gInfo@drc[1] , "2_3",'NoIDs', 'png' ), file.path( sessionPath , 'png') ))
		}

		grDevices::png( file= DRC2, width=1000, height=1000)
	
    	toPlot = data.frame(x=cellexalObj@drc[[gInfo@drc[1]]][which(!is.na(OK)),2], y=cellexalObj@drc[[gInfo@drc[1]]][which(!is.na(OK)),3], id=gr )
    	p= prettyPlot2D( toPlot, gInfo@col, showIDs = showIDs  ) #function definition in file drcPlot2D.R
    	#browser()
    	suppressWarnings(print(p)) ## write the plot
		grDevices::dev.off()

		DRC3 = file.path( sessionPath , 'png', filename(c(  gInfo@gname ,gInfo@drc[1], "1_3", 'png' ), file.path( sessionPath , 'png') )) #function definition in file 'filename.R'
		if ( ! showIDs ) {
			DRC3 = file.path( sessionPath , 'png', filename( c( gInfo@gname ,gInfo@drc[1] , "1_3",'NoIDs', 'png' ), file.path( sessionPath , 'png') ))
		}

		grDevices::png( file= DRC3, width=1000, height=1000)
	
    	toPlot = data.frame(x=cellexalObj@drc[[gInfo@drc[1]]][which(!is.na(OK)),1], y=cellexalObj@drc[[gInfo@drc[1]]][which(!is.na(OK)),3], id=gr )
    	p= prettyPlot2D( toPlot, gInfo@col, showIDs = showIDs  ) #function definition in file drcPlot2D.R
    	suppressWarnings(print(p)) ## write the plot
		grDevices::dev.off()

	}

	c( DRC1, DRC2, DRC3)
} )


prettyPlot2D = function(x, col, showIDs = TRUE){
	#x$id = as.vector(x$id)
	x[,1] = as.numeric(x[,1])
	x[,2] = as.numeric(x[,2])
	if ( length( table( x$id ) ) > length(col) ){
		x$col=  c(grey(.6),col)[as.numeric(x$id)]
	}else {
		x$col=  col[as.numeric(x$id)]
	}
    

	p = ggplot2::ggplot(x, ggplot2::aes(x=x, y=y) ) + ggplot2::theme_classic()
	p = p +   ggplot2::geom_point(color = x$col , show.legend = FALSE)

	if ( showIDs ){
	pos= t(sapply( sort(unique(x$id)), function(id) {
		ok = which(x$id == id); 
		c( stats::median(as.numeric(x[ok,1])), stats::median(as.numeric(x[ok,2])) )
	} ))
    theta <- seq(pi/8, 2*pi, length.out=48)
    xo <- diff(range(pos[,1]))/1200
    yo <- diff(range(pos[,2]))/1200
    xo1 <- diff(range(pos[,1]))/700
    yo1 <- diff(range(pos[,2]))/700
    C1 = c(grey(.6),col)
    C2 = 'black'
    rownames(pos) = names(table(x$id))
    df = t(data.frame(stringr::str_split( names(table( paste(x$id, x$col))), " ")))
    df = df[order(df[,1]),]
    for(i in theta) {
    	
    	p <- p + ggplot2::geom_text( data=data.frame(pos),
            ggplot2::aes_q(
                x = bquote(pos[,1]+.(cos(i)*xo1)),
                y = bquote(pos[,2]+.(sin(i)*yo1)),
                label=rownames(pos)[match(i, theta)] ), 
                    size=10, colour="white" )
    	
        #p <- p + ggplot2::geom_text( data=data.frame(pos),
        #    ggplot2::aes_q(
        #        x = bquote(pos[,1]+.(cos(i)*xo)),
        #        y = bquote(pos[,2]+.(sin(i)*yo)),
        #        label=sort(as.numeric(unique(x$id)) )), 
        #            size=10, colour=C2 )
        
    }
    if ( length(C1) -1 == length(unique(x$id) ) ){
    	C1 = C1[-1]
    }

    p = p + ggplot2::annotate('text', x = pos[,1], y = pos[,2],
     #label = df[,1] , size = 10, col= colorspace::lighten(df[,2], amount=.3) )  
     label = rownames(pos) , size = 10, col= 'black' )  
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
		paste( "### 2D DRC", gInfo@drc[1], "dim 1,2","(", gInfo@gname,")", addOn),"\n",
		paste("![](",drcFiles[1],")"),
		'')
	if ( ! is.na(drcFiles[2]) ){
		str = c( str, 
		paste( "### 2D DRC", gInfo@drc[1], "dim 2,3","(", gInfo@gname,")", addOn),"\n",
		paste("![](",drcFiles[2],")"),
		"")
	}
	if ( ! is.na(drcFiles[3]) ){
		str = c( str, 
		paste( "### 2D DRC", gInfo@drc[1], "dim 1,3","(", gInfo@gname,")", addOn),"\n",
		paste("![](",drcFiles[3],")"),
		"")
	}
	str
}
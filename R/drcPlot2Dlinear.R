#' drcPlots2Dlinear is a function linked to the log functionallity.
#' 
#' This function will create the 2D DRC images for the logLinear selection sections.
#' 
#' @name drcPlots2Dlinear
#' @docType methods
#' @description create two 2D drc plots for the report
#' @param cellexalObj the cellexal object
#' @param gInfo either a cellexalLinear or a cellexalGrouping object
#' @title create two 2D drc plots for the report
#' @export 
setGeneric('drcPlots2Dlinear', ## Name
	function ( cellexalObj, gInfo ) { 
		standardGeneric('drcPlots2Dlinear')
	}
)



#' @rdname drcPlots2Dlinear
setMethod('drcPlots2Dlinear', signature = c ('cellexalvrR', 'cellexalLinear'),
	definition = function ( cellexalObj, gInfo ){

		gInfo = groupingInfo( cellexalObj, gInfo@parentSelection)
		drcPlots2Dlinear(cellexalObj, gInfo )
	}
)


#' @rdname drcPlots2Dlinear
setMethod('drcPlots2Dlinear', signature = c ('cellexalvrR', 'cellexalGrouping'),
	definition = function ( cellexalObj, gInfo ) {

		cellexalObj = sessionPath(cellexalObj) #function definition in file 'sessionPath.R'
		sessionPath= cellexalObj@usedObj$sessionPath
		#print ( paste( cellexalObj@outpath, sessionPath))
		if ( ! file.exists(file.path( sessionPath , 'png') )){
			dir.create(file.path( sessionPath , 'png')  )
		}
		if ( ! gInfo@drc %in% names(cellexalObj@drc) ){
			stop( paste("group info does not match to cellexalObj data content: drc named", gInfo@drc, "not in list", paste( collapse=", ", names(cellexalObj@drc))))
		}
		
		drc = cellexalObj@drc[[gInfo@drc]]
		gInfo@order[ which(is.na(gInfo@order))] = as.integer(0)
		if ( any( ! is.numeric(gInfo@order)) ) {
			message("wrong data in gInfo@order")
		}
		if ( min(as.vector(gInfo@order)) == 0) {
			gInfo@order = as.integer(as.vector(gInfo@order) +1)
		}
		DRC1 = file.path( sessionPath , 'png', filename( c( gInfo@gname ,gInfo@drc , "1_2", 'png' ) )) #function definition in file 'filename.R'
		grDevices::png( file= DRC1, width=1000, height=1000)
		#browser()
		linearSelection = cellexalObj@usedObj$linearSelections[[paste(gInfo@gname, 'linearSelection' )]]
		if ( is.null( linearSelection) ) {
			## if it is already a Time.group we look at we are fine!
			linearSelection = cellexalObj@usedObj$linearSelections[[gInfo@gname]]
		}
		if ( is.null( linearSelection) ) {
			## if it is already a Time.group we look at we are fine!
			linearSelection = gInfo@linarObj
		}
		if ( is.null( linearSelection) ) {
			stop("I could not identify the time object?!?")
		}
		if ( is.null(linearSelection) ) {
			if (cellexalObj@usedObj$linearSelections[["lastEntry"]]@gname == gInfo@gname ){
				linearSelection = cellexalObj@usedObj$linearSelections[["lastEntry"]]
			}
			else {
				return( drcPlots2D( cellexalObj, gInfo ) )
			}
		}
		id = as.numeric(factor(color(linearSelection,rownames(drc))))
		col = color(linearSelection, rownames(drc))

   		p= prettyPlot2Dtime( data.frame(id = id, x=	drc[,1], y=	drc[,2], col = col) ) #function definition in file drcPlot2D.R
    	print(p) #write the plot
		grDevices::dev.off()


		
		#rgl::plot3d( linearSelection$x[linearSelection$time], linearSelection$y[linearSelection$time], linearSelection$z[linearSelection$time], col=gplots::bluered( length( linearSelection$x)))
		#rgl::plot3d( linearSelection$a[linearSelection$time], linearSelection$b[linearSelection$time], linearSelection$c[linearSelection$time], col=gplots::bluered( length( linearSelection$x)) )
		DRC2 = DRC3 = NULL
		if ( ! var(cellexalObj@drc[[gInfo@drc]][,3]) == 0 ) {
			DRC2 = file.path( sessionPath , 'png', filename(c(  gInfo@gname ,gInfo@drc, "2_3", 'png' ) )) #function definition in file 'filename.R'
			grDevices::png( file= DRC2, width=1000, height=1000)
			p= prettyPlot2Dtime( data.frame(id = id, x=	drc[,2], y=	drc[,3],col= col) ) #function definition in file drcPlot2D.R
    		print(p) #write the plot
			grDevices::dev.off()
			DRC3 = file.path( sessionPath , 'png', filename(c(  gInfo@gname ,gInfo@drc, "1_3", 'png' ) )) #function definition in file 'filename.R'
			grDevices::png( file= DRC3, width=1000, height=1000)
			p= prettyPlot2Dtime( data.frame(id = id, x=	drc[,1], y=	drc[,3],col= col) ) #function definition in file drcPlot2D.R
    		print(p) #write the plot
			grDevices::dev.off()
		}
		
		c( DRC1, DRC2, DRC3)
} )


#' A rather naive ggplot2 function to plot a simple Matrix.
#' @name prettyPlot2Dtime
#' @docType methods
#' @param x the data.frame containing the data for the plot
#' @title A rather naive ggplot2 function to plot a simple Matrix.
prettyPlot2Dtime = function(x ){

	x$id = as.vector(x$id)
	x[,'x'] = as.numeric(x[,'x'])
	x[,'y'] = as.numeric(x[,'y'])

	
	p = ggplot2::ggplot(x, ggplot2::aes(x=x, y=y) ) +  ggplot2::theme_classic()
	p = p +   ggplot2::geom_point(color = x$col , show.legend = FALSE)
    p
}  


#' drcFiles2HTMLlinear is a function linked to the log functionallity.
#' This function converts the file paths to Rmd image strings.
#' 
#' @name drcFiles2HTMLlinear
#' @docType methods
#' @description convert the drcPlots2D into rmd format
#' @param cellexalObj the cellexal object
#' @param gInfo the return value from cellexalvrR::groupingInfo()
#' @param showIDs here for compatibility to drcFiles2HTML
#' @param addOn a text to add in the figure heading (default NULL)
#' @title convert the drcPlots2D into rmd format
#' @export 
drcFiles2HTMLlinear = function( cellexalObj, gInfo, showIDs=TRUE, addOn = NULL ) {
	## gInfo is a list with names grouping, drc, col and order
	# create a file containing the grouping info (and thereby color) and the drc info - do not create doubles
	drcFiles =sapply( drcPlots2Dlinear( cellexalObj, gInfo ), correctPath, cellexalObj )
	str = c(
		paste( "### 2D DRC", gInfo@drc, "dim 1,2","(", gInfo@gname,")", addOn),"\n",
		paste("![](",drcFiles[1],")"),
		'',"")
	if ( ! is.na(drcFiles[2]) ){
		str = c( str, 
		paste( "### 2D DRC", gInfo@drc, "dim 2,3","(", gInfo@gname,")", addOn),"\n",
		paste("![](",drcFiles[2],")"),
		"","")
	}
	if ( ! is.na(drcFiles[3]) ){
		str = c( str, 
		paste( "### 2D DRC", gInfo@drc, "dim 1,3","(", gInfo@gname,")", addOn),"\n",
		paste("![](",drcFiles[3],")"),
		"","")
	}
	paste( str, collapse="\n", sep="\n" )
}
#' drcPlots2D is a function linked to the log functionallity.
#' 
#' This function will create the 2D DRC images for the log sections.
#' 
#' @name drcPlots2D
#' @aliases drcPlots2D,cellexalvrR-method
#' @rdname drcPlots2D-methods
#' @docType methods
#' @description create two 2D drc plots for the report
#' @param cellexalObj the cellexal object
#' @param gInfo the return value from cellexalvrR::groupingInfo()
#' @title description of function drcPlot2D
#' @export 
setGeneric('drcPlots2D', ## Name
	function ( cellexalObj, gInfo, GOIs=NULL ) { 
		standardGeneric('drcPlots2D')
	}
)

setMethod('drcPlots2D', signature = c ('cellexalvrR'),
	definition = function ( cellexalObj, gInfo, GOIs=NULL ) {

		cellexalObj = sessionPath(cellexalObj) #function definition in file 'sessionPath.R'
		sessionPath= cellexalObj@usedObj$sessionPath
		
		print ( paste( cellexalObj@outpath, sessionPath))
		if ( ! file.exists(file.path( sessionPath , 'png') )){
			dir.create(file.path( sessionPath , 'png')  )
		}
		# if ( gInfo$gname == 'Time.group.3') {
		# 	browser()
		# }
	DRC1 = file.path( sessionPath , 'png', filename( c( gInfo$gname ,gInfo$drc , "1_2", 'png' ) )) #function definition in file 'filename.R'

	gInfo$grouping = as.numeric( gInfo$grouping )

	gInfo$grouping[ which(is.na(gInfo$grouping))] = 0
	if ( any( ! is.numeric(gInfo$grouping)) ) {
		message("wrong data in gInfo$grouping")
		browser()
	}
	gInfo$grouping = as.vector(gInfo$grouping) +1
	if ( ! gInfo$drc %in% names(cellexalObj@drc) ){
		stop( paste("group info does not match to cellexalObj data content: drc named", gInfo$drc, "not in list", paste( collapse=", ", names(cellexalObj@drc))))
	}
	#if( length( grep( 'Time', gInfo$gname)) > 0 ){ browser() }
	#if ( ! file.exists( DRC1 ) ){
		#if ( gInfo$gname == 'Time.group.3') {		browser()   }
		grDevices::png( file= DRC1, width=1000, height=1000)
		graphics::plot(
				cellexalObj@drc[[gInfo$drc]][,1], cellexalObj@drc[[gInfo$drc]][,2], col= c( grey(.6) ,gInfo$col)[ gInfo$grouping ],
				main = paste( gInfo$drc, 'dim 1+2' ), xlab="dimension 1", ylab= "dimension 2" )
		grDevices::dev.off()
	#}	
	DRC2 = file.path( sessionPath , 'png', filename(c(  gInfo$gname ,gInfo$drc, "2_3", 'png' ) )) #function definition in file 'filename.R'
	#if ( ! file.exists( DRC2 ) ){
		grDevices::png( file= DRC2, width=1000, height=1000)
		graphics::plot(
				cellexalObj@drc[[gInfo$drc]][,2], cellexalObj@drc[[gInfo$drc]][,3],col= c( grey(.6) ,gInfo$col)[ gInfo$grouping ],
				main = paste( gInfo$drc, 'dim 2+3' ), xlab="dimension 2", ylab= "dimension 3" )
		grDevices::dev.off()
	#}
	c( DRC1, DRC2)
} )

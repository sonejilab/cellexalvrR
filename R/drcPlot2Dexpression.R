#' drcPlot2Dexpression is a function linked to the log functionallity.
#' 
#' This function will create the 2D DRC images for the correlated genes log sections.
#' 
#' @name drcPlot2Dexpression
#' @docType methods
#' @description create one (2D) or three 2D drc plots for the report
#' @param cellexalObj the cellexal object
#' @param drc the edcr (name) to plot on (default the first one)
#' @param GOI the genes to plot the mean expression for
#' @title create one (2D) or three 2D drc plots for the report
#' @export 
setGeneric('drcPlot2Dexpression', ## Name
	function ( cellexalObj, drc = 1, GOI ) { 
		standardGeneric('drcPlot2Dexpression')
	}
)


#' @rdname drcPlot2Dexpression
setMethod('drcPlot2Dexpression', signature = c ('cellexalvrR', 'character', 'character'),
	definition = function ( cellexalObj, drc=1, GOI ) {

		cellexalObj = sessionPath(cellexalObj) #function definition in file 'sessionPath.R'
		sessionPath= cellexalObj@usedObj$sessionPath
		
		#print ( paste( cellexalObj@outpath, sessionPath))
		if ( ! file.exists(file.path( sessionPath , 'png') )){
			dir.create(file.path( sessionPath , 'png')  )
		}
		if ( is.null(cellexalObj@drc[[drc]] )){
			stop(paste("drc", drc,"is not part of the object") )
		}
		data = NULL
		OK = match( tolower(GOI), tolower(rownames(cellexalObj@data)))
		OK = OK[which(!is.na(OK))]
		if ( length(OK) == 1 ){ ## one gene
			data = cellexalObj@data[OK,]
		}else if ( length(GOI) > 1 ){ ## mean expression
			data = as.vector(t(FastWilcoxTest::collapse( Matrix::t( cellexalObj@data[OK,] ), as.integer( rep(1, length(OK))), 1 )))
			GOI = paste(GOI[1], 'and', length(OK)-1, sep="_" )
		}else {
			stop("I need at least one gene to plot!")
		}
		brks=10
		brks <- unique(as.vector(c(0, stats::quantile(data,seq(0,1,by=1/brks)),max(data))))
		if ( brks[1] ==0 ){ 
			brks =c(-0.0001, 0.0001, brks[-1])
		}
		heapmapCols = function(x){ c("black", gplots::bluered(length(x)))}
		gr = cut(data, brks)
		gr = as.numeric(factor( gr ))
	
		## now this data needs to be z.scored

	DRC1 = file.path( sessionPath , 'png', filename( c( GOI ,drc , "1_2", 'png' ) )) #function definition in file 'filename.R'

    if ( length(cellexalObj@drc[[drc]][,1]) != length(gr) ){
    	OK = match( rownames(cellexalObj@drc[[drc]]), colnames(cellexalObj@data))
    	gr = gr[OK]
    }

	grDevices::png( file= DRC1, width=1000, height=1000)
	col = gplots::bluered(length(brks))
	toPlot = data.frame(x=cellexalObj@drc[[drc]][,1], y=cellexalObj@drc[[drc]][,2], id=gr )
	p= prettyPlot2D( toPlot, col , FALSE )
	print(p)
	
	grDevices::dev.off()
	DRC2 = DRC3= NULL

	if (  var( cellexalObj@drc[[drc]][,3]) != 0 ) {
		DRC2 = file.path( sessionPath , 'png', filename(c( GOI ,drc, "2_3", 'png' ) )) #function definition in file 'filename.R'


		grDevices::png( file= DRC2, width=1000, height=1000)
	
    	toPlot = data.frame(x=cellexalObj@drc[[drc]][,2], y=cellexalObj@drc[[drc]][,3], id=gr )
    	p= prettyPlot2D( toPlot,col , FALSE ) #function definition in file drcPlot2D.R
    	print(p) ## write the plot
		grDevices::dev.off()

		DRC3 = file.path( sessionPath , 'png', filename(c( GOI ,drc, "1_3", 'png' ) )) #function definition in file 'filename.R'


		grDevices::png( file= DRC3, width=1000, height=1000)
	
    	toPlot = data.frame(x=cellexalObj@drc[[drc]][,1], y=cellexalObj@drc[[drc]][,3], id=gr )
    	p= prettyPlot2D( toPlot, col, FALSE   ) #function definition in file drcPlot2D.R
    	print(p) ## write the plot
		grDevices::dev.off()

	}

	c( DRC1, DRC2, DRC3)
} )



#' drcFiles2HTMLexpression is a function linked to the log functionallity.
#' This function converts the file paths to Rmd image strings.
#' 
#' @name drcFiles2HTMLexpression
#' @docType methods
#' @description convert the drcPlots2D into rmd format
#' @param cellexalObj the cellexal object
#' @param drc the drc object to plot the data onto
#' @param GOI the return value from cellexalvrR::groupingInfo()
#' @param addOn a text to add in the figure heading (default NULL)
#' @title convert the drcPlots2D into rmd format
#' @export 
drcFiles2HTMLexpression = function( cellexalObj, drc, GOI, addOn = NULL ) {
	## gInfo is a list with names grouping, drc, col and order
	# create a file containing the grouping info (and thereby color) and the drc info - do not create doubles
	drcFiles =sapply( drcPlot2Dexpression( cellexalObj, drc, GOI ), correctPath, cellexalObj )
	if ( length(GOI) > 1){
		GOI= paste( GOI[1],"+",length(GOI)-1,"genes")
	}
	str = c(
		paste( "### 2D DRC", drc, "dim 1,2","(", GOI,")", addOn),"\n",
		paste("![](",drcFiles[1],")"),
		'')
	if ( ! is.na(drcFiles[2]) ){
		str = c( str, 
		paste( "### 2D DRC", drc, "dim 2,3","(", GOI,")", addOn),"\n",
		paste("![](",drcFiles[2],")"),
		"")
	}
	if ( ! is.na(drcFiles[3]) ){
		str = c( str, 
		paste( "### 2D DRC", drc, "dim 1,3","(", GOI,")", addOn),"\n",
		paste("![](",drcFiles[3],")"),
		"")
	}
	str
}
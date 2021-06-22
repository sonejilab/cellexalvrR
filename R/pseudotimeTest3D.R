#' A simple pseudotime analysis based on a linear model, that can create the pseudotime from the two varibales 'a' and 'b'
#' 
#' fit = loess( b~a )
#' 
#' The fitted values will be used as pseudotime and the pseudotiume is plotted 
#' versus the two defininge variables to the file outpath/Pseudotime.png using the grouping color.
#' Pseudotime is depicted as bluered line indicating the orientation blue -> red.
#' 
#' The pseudotime will be added into the samples table and the correlation to 
#' the pseudotime will be added to the annotation table (both replacing old data!).
#' 
#' The function returns the top/bottom (n) correlating genes.
#' 
#' The rolling mean values that are summing up only expression values from one group 
#' are colored in the group colour all others are black. 
#'  
#' @name pseudotimeTest3D
#' @docType methods
#' @description Calculate and plot pseudotime relevant information.
#' @param x a cellexalvrR object
#' @param a the first time defining vector (e.g. a MDS dimension) depricated
#' @param b the second time defining vector (e.g. a MDS dimension) depricated
#' @param c the third time defining vector (e.g. a MDS dimension) depricated
#' @param grouping a samples grouping to color the Pseudotime plot.
#' @param outpath a outpath for the files  default=sessionPath(x)
#' @param n how many genes to plot on both sides default=100
#' @param plotGenes additional default genes to plot default=NULL
#' @param smooth roling smoothing window size (default 100)
#' @param invert invert the time before usage ( default=FALSE)
#' @param cleanFolder remove all files from the outpout folder before creating new ones (default FALSE)
#' @param plotType ( 'png', 'png_high_res', 'pdf' )
#' @param summaryPlot the name of the summary plot file ( default NULL no summary plot)
#' @title Function called by getDifferentials to create pseudo times 
#' @export 
#if ( ! isGeneric('pseudotimeTest3D') ){
setGeneric('pseudotimeTest3D', ## Name
	function ( x, a=NULL,  b=NULL,c=NULL, grouping, outpath=NULL,  n=100, plotGenes=NULL, smooth=100, 
			invert=FALSE, cleanFolder=FALSE, plotType='png', summaryPlot=NULL) { 
		standardGeneric('pseudotimeTest3D')
	}
)
#}

 

#' @rdname pseudotimeTest3D
setMethod('pseudotimeTest3D', signature = c ('cellexalvrR'),
	definition = function ( x, a=NULL, b=NULL,c=NULL, grouping, outpath=NULL,  n=100, plotGenes=NULL, 
			smooth = 100, invert=FALSE, cleanFolder=FALSE, plotType='png' , summaryPlot=NULL ) {

		openPlot <- function(fname) {
			if ( plotType == 'pdf' ) {
				grDevices::pdf( file=paste(fname,'pdf', sep="."), width=10, height=10 )
			}else if (plotType == 'png_high_res' ){
				grDevices::png ( file=paste(fname,'highRes','png', sep="."), width=1600, height=1600)
			}else {
				grDevices::png ( file=paste(fname,'png', sep="."), width=800, height=800)
			}
		}
	if ( is.null( outpath )) {
		x = sessionPath(x)
		outpath = x@usedObj$sessionPath
	}
	if ( ! file.exists( outpath )) {
		dir.create( outpath )
	}else if ( cleanFolder) {
		unlink(file.path( outpath ,"*.png") )
		unlink(file.path( outpath ,"*.pdf") )
	}

	## base this on slingshot's implementation.
	## But that needs groups.

	info = groupingInfo(x, grouping )
	loc = reduceTo( x, 'col', to=colnames(x@data)[which(! is.na( info@grouping))])

	if ( is.null( x@drc[[info@drc]] )){
		stop( paste("the source drc", info@drc, "could not be found in the object.") )
	}
	drc = loc@drc[[info@drc]][,1:3]
	colnames(drc) = c('a', 'b' ,'c')

	res = new('cellexalLinear', dat= data.frame(drc), drc=info@drc)
	info = groupingInfo(loc, grouping )
	res = createTime( res, info )

	## add the time as group:
	#the VR program dependeds on it

	x = addSelection( res, x, grouping )


	## does the time look ok when copied to the cellexal object?
	#fnames = drcPlots2Dlinear( x, groupingInfo(x, colnames(x@userGroups)[ncol(x@userGroups) -1]) )
	#system( paste('display', fnames[1]))

	## create the .time selection file for cellexalVR
	f= file.path( x@outpath, basename(info@selectionFile)) 
	f= paste(sep=".", f, 'time')
	exportSelection( res, f )

	if ( file.exists( x@usedObj$sessionPath ) ) {
		file.copy( f, x@usedObj$sessionPath)
		file.copy( paste( sep=".",f,'points'), x@usedObj$sessionPath)
	}

	return(x)
	
} )

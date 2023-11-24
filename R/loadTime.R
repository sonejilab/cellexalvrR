#' instead of reading a saved time table into a multigroup 
#' selection this function loads the data as a cellexalLinear object.
#' 
#' @name loadTime
#' @docType methods
#' @description load a time selection file as timeline - not as grouping
#' @param cellexalObj, cellexalvr object
#' @param file the file with the stored timeline selection 
#' @title load a time selection file
#' @return an unpdated cellexalvrR object
#' @export 
#if ( ! isGeneric('loadTime') ){
setGeneric('loadTime', ## Name
	function ( cellexalObj, file=NULL ) { 
		standardGeneric('loadTime') 
	}
)
#}

#' @rdname loadTime
setMethod('loadTime', signature = c ('cellexalvrR', 'character'), 
	definition = function ( cellexalObj, file=NULL ) {
		if ( is.null(file) ){
			stop("I need a file to load the data from")
		} 
		if ( ! file.exists( file )) {
			stop(paste( sep="","the file '",file,"' does not exists!") )
		}
		data = read.delim( file=file, sep="\t", row.names=1, header=F )
		drc = data[1,2]
		if ( is.null(cellexalObj@drc[[drc]])) {
			stop("the drc this time was selected on does not exists in this object")
		}
		cell_I_want = match ( rownames(data), colnames(cellexalObj@data) )
		if ( length(is.na(m)) > 0 ){
			stop("The selection describes cells I do not have!")
		}
		time = new('cellexalLinear', dat= data.frame(cellexalObj@drc[[drc]][rownames(data),]), drc=drc)
		# the time is not saved - so here we need to assume even spacing
		time@dat$time = 1:nrow(data)
		time@dat$order = 1:nrow(data)
		time@id = digest::digest( time@dat, algo="md5")
		x@dat$col = factor(wesanderson::wes_palette("Zissou1", 10, type = "continuous")[ 1:nrow(data) ], 
		levels= wesanderson::wes_palette("Zissou1", 10, type = "continuous") )

		addSelection( cellexalObj, time )

		cellexalObj
	}
)
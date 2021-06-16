
#' The cellexalvrR sanity check.
#' Prints out the problems and sets a x@usedObj$ceckPassed boolean value.
#' This function is called in the export2cellexalvr function and others.
#'
#' @name check
#' @docType methods
#' @description checks cellexalvrR internals
#' @param x the cellexalvrR object
#' @param silent do not report warnings (default FALSE)
#' @title cellexal internal checks
#' @returns the checked cellexalvrR object
#' @export 
setGeneric('check', ## Name
	function ( x, silent=FALSE ) {
		standardGeneric('check')
	}
)



#' @rdname check 
setMethod('check', signature = c ('cellexalvrR'),
	definition = function ( x, silent=FALSE ) {
	
	oldw <- getOption("warn")
	if ( silent ) {
    	options(warn = -1)
	}
	cn = colnames(x@data)
	rn = rownames(x@data)

	OK = FALSE
	error = NULL;

	# meta.gene
	if ( ! is(x@meta.gene, 'matrix') ) {
		error = c( error, 'the meta.gene slot does not contain a matrix object!')
	}
	if ( nrow(x@meta.gene) != nrow(x@data)) {
		if ( ! silent ) {
			warning("meta.gene object re-created from rownames")
		}
		x@meta.gene = matrix( rownames(x@data), ncol=1)
		colnames( x@meta.gene) = 'Gene Symbol'
	}
	if ( ncol(x@meta.gene) == 1) {
		## ooh that is bad - lets get a dummy one in, too
		x@meta.gene = cbind( x@meta.gene, rep(1, nrow(x@meta.gene)))
		colnames(x@meta.gene)[2] = "savekeeping"
	}

	if ( ! all(all.equal(rownames(x@data), rownames( x@meta.gene)) == TRUE) ) {
		if ( nrow(x@meta.gene) == nrow(x@data) ){
			## likely correct
			warning("meta.gene rownames set to data rownames")
			rownames(x@meta.gene) = rownames(x@data)
		}else{
			error = c( error,"the data rownames are not the same as the meta.gene rownames!")
		}
	}

	# meta.cell
	if ( ! isTRUE(all( all.equal(rownames(x@meta.cell), cn) == TRUE ) ) ){
		error = c(error,"the data colnames are not the same as the meta.cell rownames!")
	}
	
	testMatrix <- function ( x ) {
		OK = TRUE
		if ( ! isTRUE( all.equal(names(table(x)), c("0","1"))) ) {
			if ( isTRUE( all.equal(names(table(x)), c("1"))) | isTRUE( all.equal(names(table(x)), c("0")))  ){
				OK = TRUE
			}else {
				OK = FALSE
			}
		}
		OK
	}

	if ( !  all(apply(x@meta.cell,2, testMatrix), TRUE) ){
		error = c( error,"meta.cells is not a 0/1 table")
	}
	
	## the 3D models

	for( n in names( x@drc ) ){
		OK = TRUE
		## there seams to be an issue created during R object maniuplations creating NA rows.
		## I need to get rid of them here!
		#bad = which(apply(x@drc[[n]], 1, function(x){ all( is.na(x), TRUE ) } ))
		
		#if ( length(bad) > 0){
		#	x@drc[[n]] = x@drc[[n]][-bad,]
		#}
		bad = which(apply(x@drc[[n]], 1, function(x){ all( is.na(x), TRUE ) } ))
		if ( length(bad) > 0 ) {	
			# just fix that here - no need to brag about it...
			x@drc[[n]] = x@drc[[n]][-bad,]
			#error = c(error , 
			#	paste("R logics ERROR: NA's in the drc", n ,
			#		"rownames - please fix that") )
			#browser()
			#OK =FALSE
		}
		if ( ! isTRUE(all.equal(rownames(x@drc[[n]]), cn) ) ){
			if ( nrow( x@drc[[n]]) == length(cn) ){
				if ( isTRUE(all.equal(sort(rownames(x@drc[[n]])), sort(cn))) ){
					#error = c(error , 
					#paste("drc", n ,
					#	"wrong cell order") )
					## reorder that!
					m = match( cn, rownames(x@drc[[n]]))
					x@drc[[n]] = x@drc[[n]][m[which(!is.na(m))],]
				
				}else if( is.null(rownames(x@drc[[n]])) ) {
					error = c(error , 
					paste("drc", n ,
						"has no rownames - please fix that") )
					}
				else{
					error = c(error , 
					paste("cell name missmatch - different cell names drc", n ,
						" - are you sure the drc is from this data?") )
				}
				OK = FALSE
			}
		}
		if ( length(which(is.na(x@drc[[n]]))) > 0){
			error = c(error , 
					paste("NA values in drc", n ,
						" - please fix that") )
		}
	}

	# the timelines (if some exist)
	if ( ! is.null(x@usedObj$timelines)){
		for (n in names(x@usedObj$timelines)){
			x@usedObj$timelines[[n]] = checkTime( x@usedObj$timelines[[n]], x )
		}
	}

	for ( g in names(x@groupSelectedFrom)){
		if ( !is(x@groupSelectedFrom[[g]], 'cellexalGrouping')){
			error = c(error , 
					paste("groupSelectedFrom",g,"is no cellexalGrouping"))
		}
	}
	
	if ( !is.null(error) ){
		x@usedObj$checkPassed = FALSE
		x@usedObj$checkError = error
		if ( ! silent ){
			message(paste(collapse=" \n\r",c ( "check cellexalvrR", error) ) )
		}
	}else {
		x@usedObj$checkPassed = TRUE
		x@usedObj$checkError = error
		if ( ! silent ) {
			message("object passes checks")
		}
	}
	
	options(warn = oldw)
	invisible( x )
} )

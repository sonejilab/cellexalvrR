#' check  cellexalvrR sanity check.
#' prints out the problems and sets a x@usedObj$ceckPassed boolean value
#' This function is called in the export2cellexalvr function.
#'
#' @name check
#' @aliases check,cellexalvrR-method
#' @rdname check-methods
#' @docType methods
#' @description checks cellexalvrR internals
#' @param x the cellexalvrR object
#' @title cellexal internal checks
#' @returns the checked cellexalvrR object
#' @export 

setGeneric('check', ## Name
	function ( x ) {
		standardGeneric('check')
	}
)

setMethod('check', signature = c ('cellexalvrR'),
	definition = function ( x ) {
	
	cn = colnames(x@data)
	rn = rownames(x@data)

	OK = FALSE
	error = NULL;

	# meta.gene
	if ( ! is(x@meta.gene, 'matrix') ) {
		error = c( error, 'the meta.gene slot does not contain a matrix object!')
	}
	if ( ncol(x@meta.gene) == 1) {
		## ooh that is bad - lets get a dummy one in, too
		x@meta.gene = cbind( x@meta.gene, rep(1, nrow(x@meta.gene)))
		colnames(x@meta.gene)[2] = "savekeeping"
	}

	if ( isFALSE( all.equal( rn, rownames(x@meta.gene)) ) ) {
		error = c( error,"the data rownames are not the same as the meta.gene rownames!")
	}

	# meta.cell
	if ( ! isTRUE( all.equal(rownames(x@meta.cell), cn) ) ){
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
			if ( length(which(is.na(match(rownames(x@drc[[n]]), cn )))) > 0 ) {
				error = c(error , 
					paste("R logics ERROR: NA's in the drc", n ,
						"rownames - please fix that") )
				#browser()
				OK =FALSE
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
			checkTime(x, x@usedObj$timelines[[n]] )
		}
	}
	
	if ( !is.null(error) ){
		x@usedObj$checkPassed = FALSE
		x@usedObj$checkError = error
		message(paste(collapse=" \n\r",c ( "check cellexalvrR", error) ) )
	}else {
		x@usedObj$checkPassed = TRUE
		x@usedObj$checkError = error
		message("seams OK")
	}
	

	invisible( x )
} )

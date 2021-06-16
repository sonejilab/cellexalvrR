#' Filename simple pastes a vector of characters using '.' and removes all whitespace.
#'
#' Simple function to unclutter the R code.
#' @name filename
#' @docType methods
#' @description simple function to join by '.' and remove whitespace.
#' @param str a string vector
#' @title create a filename without spaces from a list of strings
#' @export 
setGeneric('filename', ## Name
	function ( str ) { 
		standardGeneric('filename')
	}
)



#' @rdname filename
setMethod('filename', signature = c ('character'),
	definition = function ( str ) {
	of = paste( collapse=".", str )
	of = stringr::str_replace_all(of, "\\s+",'_')
	stringr::str_replace_all(of, "[:-]",'_')
} )

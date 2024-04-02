#' Filename simple pastes a vector of characters using '.' and removes all whitespace.
#'
#' Simple function to unclutter the R code.
#' @name filename
#' @docType methods
#' @description simple function to join by '.' and remove whitespace.
#' @param str a string vector like c('plot','png')
#' @param path an optional path - if given the function returns a not existing file name
#' @title create a filename without spaces from a list of strings
#' @export 
setGeneric('filename', ## Name
	function ( str, path ) { 
		standardGeneric('filename')
	}
)



#' @rdname filename
setMethod('filename', signature = c ('character', 'missing'),
	definition = function ( str, path=NULL ) {
	of = paste( collapse=".", str )
	of = stringr::str_replace_all(of, "\\s+",'_')
	stringr::str_replace_all(of, "[:-]",'_')
} )


#' @rdname filename
setMethod('filename', signature = c ('character', 'character'),
	definition = function ( str, path ) {
		#print (paste("file path", path, "str: ", str) )
		if ( file.exists(file.path( path, filename(str)))){
			id = 1
			ext = length(str)
			while( file.exists(file.path( path, filename(c(str[-ext], id, str[ext] ))) ) ){
				id = id + 1
			}
			return( filename(c(str[-ext], id, str[ext] )) )
		}
		filename(str)
} )
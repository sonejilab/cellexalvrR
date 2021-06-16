#' Create a R markdown link used in the (VR) log files.
#' 
#' @name rmdLink
#' @docType methods
#' @description creates a linke in the structure [<name>](<link><name>){target='blank'}
#' @param name the displayed text
#' @param link the link address
#' @param lineEnd add a line end at the end of every entry (default =T) 
#' @title easily create an Rmd link that opens in a new window.
#' @export 
#if ( ! isGeneric('renew') ){
setGeneric('rmdLink', ## Name
			function ( name, link, lineEnd = T ) { 
				standardGeneric('rmdLink')
			}
)
#}


#' @rdname rmdLink
setMethod('rmdLink', signature = c ('character'),
		definition = function ( name, link, lineEnd = T  ) {
			
			if ( lineEnd ){
				if(substr(Sys.getenv("OS"),1,7) == "Windows") {
					# set Windows newline
					newLine <- "\r\n"
				}
				else {
					# set non-Windows newline
					newLine <- "\n"
				}
				paste( sep="", "[", name,"](",link,name,")",newLine)
			}else {
				paste( sep="", "[", name,"](",link,name,")")
			}
	} 
)

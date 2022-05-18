#' @name show
#' @rdname show
#' @docType methods
#' @description  show the cellexalvrR
#' @param object the cellexalvr object
#' @return nothing at all
#' @title show the cellexalvrR objects's summary
#' @export show
setMethod('show', signature = c ('cellexalvrR'),
		definition = function (object) {
			cat ( toString( object ) )
			# cat (paste("An object of class", class(object)),"\n" )
			# #cat("named ",object@name,"\n")
			# cat (paste( 'with',nrow(object@data),'genes and', ncol(object@data),' cells.'),"\n")
			# cat (paste("Annotation datasets (",paste(dim(object@meta.gene),collapse=','),"): '",paste( colnames(object@meta.gene ), collapse="', '"),"'  ",sep='' ),"\n")
			# cat (paste("Sample annotation (",paste(dim(object@meta.cell),collapse=','),"): '",paste( colnames(object@meta.cell ), collapse="', '"),"'  ",sep='' ),"\n")
			# cat ( paste("There are",length(grep('order', colnames(object@userGroups), invert=T)), "user group(s) stored" ),":\n")
			# if ( ncol(object@userGroups) > 0 ) {
			# 	cat ( paste( collapse=", ", colnames(object@userGroups)), '\n' )
			# }
			# if ( length(names(object@drc)) > 0 ){
			# 	cat ( "and", length(names(object@drc)), "drc object(s)\n")
			# }
			# cat (paste("Specie is set to", object@specie),"\n")
			
		}
)



#' @name toString
#' @rdname toString
#' @docType methods
#' @description create the string that is the output of show
#' @param x the cellexalvr object
#' @return the summary of the object
#' @title toString the cellexalvrR objects's summary
#' @export toString
setMethod('toString', signature = c ('cellexalvrR'),
		definition = function (x) {
			str = c(paste("An object of class", class(x)),
				paste( 'with',nrow(x@data),'genes and', ncol(x@data),' cells.'),
				paste("Annotation datasets (",paste(dim(x@meta.gene),collapse=','),"): '",
					paste( colnames(x@meta.gene ), collapse="', '"),"'  ",sep='' ),
				paste("Sample annotation (",paste(dim(x@meta.cell),collapse=','),"): '",
					paste( colnames(x@meta.cell ), collapse="', '"),"'  ",sep='' ),
				paste("There are",length(grep('order', colnames(x@userGroups), invert=T)), 
					"user group(s) stored" )
			)

			if ( ncol(x@userGroups) > 0 ) {
				str = c( str, paste( collapse=", ", colnames(x@userGroups)) )
			}
			if ( length(names(x@drc)) > 0 ){
				str = c( str, paste( "and", length(names(x@drc)), "drc object(s):",
					paste( collapse=", ", names(x@drc)) ) )
			}
			if ( length(names(x@groupSelectedFrom)) >0 ){
				OK = grep( 'pseudoBulk' ,names(x@groupSelectedFrom), invert=T )
				str = c( str, paste( "and", length(OK), "active grouping object(s):",
					paste( collapse=", ", names(x@groupSelectedFrom)[OK]) ) )
			}
			str = c(str, paste("Specie is set to", x@specie),"" )

			return (paste(collapse="\n", sep=" ",str) )
		}
)
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
			cat (paste("An object of class", class(object)),"\n" )
			#cat("named ",object@name,"\n")
			cat (paste( 'with',nrow(object@data),'genes and', ncol(object@data),' cells.'),"\n")
			cat (paste("Annotation datasets (",paste(dim(object@meta.gene),collapse=','),"): '",paste( colnames(object@meta.gene ), collapse="', '"),"'  ",sep='' ),"\n")
			cat (paste("Sample annotation (",paste(dim(object@meta.cell),collapse=','),"): '",paste( colnames(object@meta.cell ), collapse="', '"),"'  ",sep='' ),"\n")
			cat ( paste("There are",length(grep('order', colnames(object@userGroups), invert=T)), "user group(s) stored" ),":\n")
			if ( ncol(object@userGroups) > 0 ) {
				cat ( paste( collapse=", ", colnames(object@userGroups)), '\n' )
			}
			if ( length(names(object@drc)) > 0 ){
				cat ( "and", length(names(object@drc)), "drc object(s)\n")
			}
			cat (paste("Specie is set to", object@specie),"\n")
			
		}
)

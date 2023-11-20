#' Parse a stats table into a format that can be uploaded to 
#' toppgenes to look at differential pathway activity.
#' 
#' @name getSigGenes
#' @docType methods
#' @description Identify to n diff genes per cluster
#' @param tab, cellexalvrR object
#' @param count the number of to diff genes per cluster (default 500)
#' @keywords DEGs
#' @title convert stats table into ToppCluster input file
#' @examples 
#' \dontrun{
#' getSigGenes( cellexalObj@usedObj$sigGeneLists$Cpp$User.group.1 )
#' }
#' @return returns a table with two columns 1: the genes and 2: the group the genes are overexpressed in
#' @export 
#if ( ! isGeneric('getSigGenes') ){
setGeneric('getSigGenes', ## Name
			function (tab, count=500, nameAdd=NULL ) { 
				standardGeneric('getSigGenes') 
			}
	) 
#}


#' @rdname getSigGenes
setMethod('getSigGenes', signature = c ('matrix'),
	definition = function( tab, count=500, nameAdd=NULL ){
		getSigGenes( as.data.frame(tab), count=count, nameAdd=nameAdd )
	}
)

#' @rdname getSigGenes
setMethod('getSigGenes', signature = c ('data.frame'),
		definition = function( tab, count=500, nameAdd=NULL ){
			ret = NULL
			if ( ! is.na( match ('cor', colnames(tab)))){
				# crap a linear selection!
				ret = data.frame( 
					genes = c( rownames(tab)[1:round(count/2)], rownames(tab)[ (nrow(tab)-round(count/2)+1):nrow(tab)] ),
					group = c( rep('down', round(count/2)), rep('up', round(count/2)))
					)
			}
			else {
				info = split( tab$gene, tab$cluster )
				ret = data.frame( 
					genes= unlist(lapply( info, function(x){ c = count; if (c > length(x)){c = length(x)}; return(x[1:c])  })),
					group= paste(sep="", nameAdd , unlist(lapply( names(info), function(x){ c = count; if (c > length(info[[x]])){c = length(info[[x]])}; return( rep(x, c) )  })) )
				)
			}
			return (ret)
} )
#' Export the data stored in this object as GSEA compatible csv table.
#' It is not recommended to apply this to a single cell data set. 
#' Instead of this rather create pseudo bulk expression data and export this data.
#' This summed up data will be much smaller and GSEA analysis is also used to be applied to bulk data.
#' @name toGSEA
#' @docType methods
#' @description  An easy function to register the inbuilt (G)enes (O)f (I)nterest lists.
#' @param x the cellexalvrR object
#' @param ofile the file to store the cvs table in
#' @param gname the optional group to create the pseudo bulk samples for 
#'              The grouping ID can be used to analyze the data in GSEA later
#' @title easily identify e.g. all TFs in the dataset
#' @export 
#if ( ! isGeneric('toGSEA') ){
setGeneric('toGSEA', ## Name
	function (x, ofile, gname=NULL ) { 
		standardGeneric('toGSEA') 
	}
)
#}


#' @rdname toGSEA
setMethod('toGSEA', signature = c ('cellexalvrR', 'character', 'character'),
	definition = function (x, ofile, gname ) {
		gname = groupingInfo( x, gname )
		return( toGSEA( x, ofile, gname ) )
	}
)


#' @rdname toGSEA
setMethod('toGSEA', signature = c ('cellexalvrR', 'character', 'cellexalGrouping'),
	definition = function (x, ofile, gname ) {
		ofile = paste(sep=".", ofile, 'gct')
		
		loc = reduceTo( x, what = 'col', to=names( gname@grouping[which ( ! is.na( gname@grouping)) ] ) )
		#loc = pseudoBulk ( gname,  loc )
		
		df = cbind( 'NAME'=rownames(loc@data), 'Description' = rep('NA', nrow(loc@data)), as.matrix(loc@data) )
		gname =groupingInfo( loc, gname@gname )
		cn = colnames(loc@data)
		cn = unlist (stringr::str_replace_all( cn, '\\.', '_' ) )
		colnames(df) =c( 'NAME', 'Description', paste(gname@grouping, cn, sep="." ) )

		if ( file.exists(ofile) ){
			unlink(ofile)
		}
		fileConn<-file( ofile )
		writeLines(c("#1.2",paste( nrow(loc@data), ncol(loc@data), sep="\t") ), fileConn)
		close(fileConn)
		write.table( df, file=ofile, sep="\t", quote=F, row.names=F, append=T)
		return (ofile)
	}
)

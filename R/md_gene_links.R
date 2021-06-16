#' convert a list of genes into a collapsed markdown text that the user can expand.
#'
#' @name md_gene_links
#' @docType methods
#' @description Convert genes to a web link in the log files 
#' @param genes a list of strings added to "https://www.genecards.org/cgi-bin/carddisp.pl?gene="
#' @param label defaults to "Click to expand gene list"
#' @title log specific formating of a (long) list of strings
#' @export 
setGeneric('md_gene_links', ## Name
	function ( genes, label="Click to expand gene list" ) { 
		standardGeneric('md_gene_links')
	}
)



#' @rdname md_gene_links
setMethod('md_gene_links', signature = c ('character'),
	definition = function ( genes, label="Click to expand gene list" ) {

		text = paste( collapse=" ", sep=" ",
		 	paste( sep=" ", unlist( lapply(genes, function(n) { 
		 		rmdLink(n, "https://www.genecards.org/cgi-bin/carddisp.pl?gene=", lineEnd=FALSE )  })) 
		 	) ,
		"\n")

		## https://gist.github.com/pierrejoubert73/902cc94d79424356a8d20be2b382e1ab
		text = paste(sep="\r\n", collapse=" ",
			"<details>", 
			paste(sep="","  <summary>",label,"</summary>"),
			 "", text,
			 "</details>","",""
		)
		text
	}
)


#' @rdname md_gene_links
setMethod('md_gene_links', signature = c ('NULL'),
	definition = function ( genes, label="Click to expand gene list" ) {
		return("")
	}
)
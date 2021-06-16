
#' It sets the specie slot to the user supplied specie string.
#' 
#' @name set.specie
#' @docType methods
#' @description Set the specie information in the cellexalvrR object
#' @param cellexalObj cellexalvr object
#' @param specie The specie required options: "mouse" or "human"
#' @title Set the specie information in the cellexalvrR object
#' @keywords TFs
#' @export
#if ( ! isGeneric('set.specie') ){
setGeneric('set.specie', ## Name
	function (cellexalObj, specie=c("mouse","human")) { 
		standardGeneric('set.specie') 
	}
)
#}


#' @rdname set.specie
setMethod('set.specie', signature = c ('cellexalvrR'),
	definition = function (cellexalObj, specie=c("mouse","human")) {
	
	if(specie=="mouse"){
		#data(mouse.tfs)
		cellexalObj@tfs <- cellexalvrR::mouse.tfs
	}
	
	if(specie=="human"){
		#data(human.tfs)
		cellexalObj@tfs <- cellexalvrR::human.tfs
	}

	cellexalObj@specie <- specie
	cellexalObj
} )

#' get.genes.cor.to is able to correlate gene expression in 
#' a cellexalvrR object to the expression of either a gene or a FACS marker.
#' 
#' The function is used in the VR process to calculate correlating genes in the keybord list view.
#' 
#' @name  get.genes.cor.to
#' @docType methods
#' @description  Indentify positively and negatively correlated genes to a chosen gene
#' @param cellexalObj A cellexalvr object
#' @param gname The required gene
#' @param output the outfile
#' @param is.smarker Whether the supplied gene is a surface marker (default =F)
#' @param cpp use the c++ cor implementation (default = TRUE)
#' @title Indentify positively and negatively correlated genes to a chosen gene
#' @keywords correlation
#' @examples
#' print (get.genes.cor.to ( cellexalObj, 'Gata1'))
#' @export 
#if ( ! isGeneric('get.genes.cor.to') ){
setGeneric('get.genes.cor.to', ## Name
	function (cellexalObj, gname, output=NULL, is.smarker=F, cpp=T) { 
		standardGeneric('get.genes.cor.to') 
	}
)
#}



#' @rdname get.genes.cor.to
setMethod('get.genes.cor.to', signature = c ('cellexalvrR'),
	definition = function (cellexalObj, gname, output=NULL, is.smarker=F, cpp=T) {
	
	cellexalObj <- loadObject(cellexalObj) #function definition in file 'lockedSave.R'
	data <- cellexalObj@data
	rownames(data) <- tolower(rownames(data))
	
	goi <- NULL

	if(is.smarker==F){
		goi <- data[tolower(gname),]
	}
	
	if(is.smarker==T){
		m <- match( tolower(gname), tolower(colnames(cellexalObj@index)))
		

		if(is.na(m)==T)
			stop("Gene name is not an available surface marker")

		goi <- cellexalObj@index[,m]
	}
	
	if ( is.null(goi)) {
		stop( paste("the gname", gname, "is neither a gene nor a fascs maker name", paste( colnames(cellexalObj@index), collapse=", ")) )
	}
	
	calc.cor <- function(v, comp){
		stats::cor(v, comp)
	}
	if ( cpp ) {
	  cor.values <-  FastWilcoxTest::CorMatrix( data, goi)
	  names(cor.values) = rownames(data)
  	}else {
		cor.values <- apply(data,1,calc.cor,comp=goi)
	}
	
	ord <- names(sort(cor.values))
	
	pos <- ord[ (length(ord)-1): (length(ord)-10) ]
	neg <- ord[1:10]
	tab <- cbind(pos,neg)
	if ( ! is.null(output)){
		utils::write.table(t(tab),output,row.names=F,col.names=F,sep="\t",quote=F)
	}else {
		return ( tab )
	}

	if ( ! is.null(cellexalObj@usedObj$sessionPath) ) {
		opath = file.path(cellexalObj@usedObj$sessionPath, 'tables'  )
		if ( ! file.exists( opath )){
			dir.create(opath)
		}
		opath = file.path( opath, paste(sep=".", 'Cor', gname,'txt'))
		cor.values = cor.values[rev(order(cor.values))]
		utils::write.table( 
			cbind( 'Gene Name' = rev(ord), 'CorValue' = cor.values), 
			file= opath, row.names=F, quote=F, sep="\t")
		drc = names(cellexalObj@drc)[1]
		if ( length( cellexalObj@groupSelectedFrom) > 0 ){
			drc = groupingInfo(cellexalObj)@drc
		}
		rn = rownames(cellexalObj@data)
		pos = rn[match( pos, tolower(rn))]
		neg = rn[match( neg, tolower(rn))]
		Text = paste( sep=" ", collapse=" ",
			"## Genes correlated to", gname,"\n\n",
			"Max correlation value is",round(cor.values[2],3),"and min correlation value is", round(rev(cor.values)[1],3),".",
			"All Correlation data can be downloaded from [here](", file.path( cellexalObj@usedObj$sessionName,'tables', basename(opath ) ),")","\n\n",
			"\n### Genes positively correlated to", gname,"\n\n"
		)
		Top_Figs = paste( collapse=" ",drcFiles2HTMLexpression(cellexalObj, drc, pos ))
		Top_genes =	paste( collapse=" ", sep=" ",md_gene_links ( pos ))
		Bottom_Figs = paste( collapse=" ", sep=" ",drcFiles2HTMLexpression(cellexalObj, drc, neg ))
		Bottom_genes = paste( collapse=" ", sep=" ",md_gene_links ( neg ))
		Text = paste( sep="\n", collapse="\n",
			Text, Top_Figs, Top_genes,
			paste("\n### Genes negatively correlated to", gname,"\n\n"),
			Bottom_Figs, Bottom_genes, "\n"
		)
		
		cellexalObj = storeLogContents( cellexalObj, Text, type="GeneGeneCor")
		id = length(cellexalObj@usedObj$sessionRmdFiles)
		cellexalObj = renderFile( cellexalObj, id, type="GeneGeneCor" )


	}

	invisible( tab )
} )


#' @rdname get.genes.cor.to
setMethod('get.genes.cor.to', signature = c ('character'),
		definition = function (cellexalObj, gname, output=NULL, is.smarker=F, cpp=T) {
			cellexalObj <- loadObject(cellexalObj) #function definition in file 'lockedSave.R'
			get.genes.cor.to( cellexalObj, gname, output, is.smarker, cpp) #function definition in file 'cellexalvrFunctions.R'
		}
)

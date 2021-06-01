
#' set.specie is depricated
#' 
#' It simply sets the specie slot to the user supplied specie string.
#' 
#' @name set.specie
#' @aliases set.specie,cellexalvrR-method
#' @rdname set.specie-methods
#' @docType methods
#' @description  Loads TF annotation into cellexalvr object
#' @param cellexalObj, cellexalvr object
#' @param specie The specie required options: "mouse" or "human"
#' @title description of function set.specie
#' @keywords TFs
#' @export set.specie
#if ( ! isGeneric('renew') ){
setGeneric('set.specie', ## Name
	function (cellexalObj, specie=c("mouse","human")) { 
		standardGeneric('set.specie') 
	}
)
#}

setMethod('set.specie', signature = c ('cellexalvrR'),
	definition = function (cellexalObj, specie=c("mouse","human")) {
	
	if(specie=="mouse"){
		#data(mouse.tfs)
		cellexalObj@tfs <- mouse.tfs
	}
	
	if(specie=="human"){
		#data(human.tfs)
		cellexalObj@tfs <- human.tfs
	}

	cellexalObj@specie <- specie
	cellexalObj
} )


#if ( ! isGeneric('renew') ){
setGeneric('get.genes.cor.to', ## Name
	function (cellexalObj, gname, output=NULL, is.smarker=F, cpp=T) { 
		standardGeneric('get.genes.cor.to') 
	}
)
#}


#' get.genes.cor.to is able to correlate gene expression in 
#' a cellexalvrR object to the expression of either a gene or a FACS marker.
#' 
#' The function is used in the VR process to calculate correlating genes in the keybord list view.
#' 
#' @name  get.genes.cor.to
#' @aliases get.genes.cor.to,cellexalvrR-method
#' @rdname get.genes.cor.to-methods
#' @docType methods
#' @description  Gets positively and negatively correlated genes to a chosen gene
#' @param cellexalObj A cellexalvr object
#' @param gname The required gene
#' @param output the outfile
#' @param is.smarker Whether the supplied gene is a surface marker (default =F)
#' @param cpp use the c++ cor implementation (default = TRUE)
#' @title description of function get.genes.cor.to
#' @keywords correlation
#' @examples
#' print (get.genes.cor.to ( cellexalObj, 'Gata1')) #function definition in file 'cellexalvrFunctions.R'
#' @export get.genes.cor.to
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
		return ( tab)
	}
	invisible(tab)
} )

#' @describeIn get.genes.cor.to cellexalvrR
#' @docType methods
#' @description  preload the cellexalObj
#' @param cellexalObj the cellexalObj.RData file
#' @param gname The required gene
#' @param output the outfile
#' @param is.smarker Whether the supplied gene is a surface marker (default =F)
#' @param cpp use the c++ cor implementation (default = TRUE)
#' @title description of function get.genes.cor.to (character) #function definition in file 'cellexalvrFunctions.R'
#' @export 
setMethod('get.genes.cor.to', signature = c ('character'),
		definition = function (cellexalObj, gname, output=NULL, is.smarker=F, cpp=T) {
			cellexalObj <- loadObject(cellexalObj) #function definition in file 'lockedSave.R'
			get.genes.cor.to( cellexalObj, gname, output, is.smarker, cpp) #function definition in file 'cellexalvrFunctions.R'
		}
)

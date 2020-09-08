#' The ontology analysis for the log files.
#' @name ontologyLogPage
#' @aliases ontologyLogPage,cellexalvrR-method
#' @rdname ontologyLogPage-methods
#' @docType methods
#' @description creates the GO analysis for a gene list and puts it into the report.
#' @param cellexalObj the cellexalvrR object
#' @param genes a list of gene symbols (IMPORTANT)
#' @param grouping the grouping this gene list originated on (default = NULL; use last grouping)
#' @param ontology which GO ontology to choose from (default = "BP")
#' @param topNodes how many GO terms to report (default 10)
#' @param ... unused
#' @import org.Mm.eg.db org.Hs.eg.db topGO
#' @title description of function ontologyLogPage
#' @export
setGeneric('ontologyLogPage', ## Name
	function ( cellexalObj, genes, grouping=NULL, ontology = 'BP', topNodes=10, ... ) {
		standardGeneric('ontologyLogPage')
	}
)

setMethod('ontologyLogPage', signature = c ('cellexalvrR'),
	definition = function ( cellexalObj, genes, grouping=NULL, ontology = 'BP', topNodes=10, ... ) {
	## process the ontology for this gene list and add one ontology report page
	#requireNamespace( 'AnnotationDbi' )
	require( 'topGO' )
	
	if ( file.exists(genes)) {
		genes = as.vector(utils::read.delim(genes)[,1])
	}
	if ( is.null( grouping )) {
		grouping = cellexalObj@usedObj$lastGroup
	}
	cellexalObj = userGrouping( cellexalObj, grouping ) #function definition in file 'userGrouping.R'
	cellexalObj = sessionRegisterGrouping( cellexalObj, cellexalObj@usedObj$lastGroup ) #function definition in file 'sessionRegisterGrouping.R'
	
	#error = ""
	# message( paste( sep=" ","ontologyLogPage genes:",  paste( sep=", ",genes) ) )
	## for this to work as expected you need an up to datae pandoc:
	## https://pandoc.org/installing.html

	n = length( grep ( "GOanalyis.csv", list.files( file.path(cellexalObj@usedObj$sessionPath, 'tables') ) ) ) +1
	
	if( length(cellexalObj@specie) == 0){
		cellexalObj = useInbuiltGOIlists(cellexalObj, 'TFs' ) ## sets the species if not alread set #function definition in file 'useInbuiltGOIlists.R'
	}
	if(cellexalObj@specie =='mouse'){
		if( requireNamespace('org.Mm.eg.db')){
			x <- org.Mm.eg.db
		}else{
			stop("Install org.Mm.eg.db package for retrieving gene lists from GO")
		}
	}else if ( cellexalObj@specie=='human'){
		if( requireNamespace('org.Hs.eg.db')){
			x <- org.Hs.eg.db
		}else{
			stop("Install org.Hs.eg.db package for retrieving gene lists from GO")
		}
	}else {
		stop( paste( "The specie",  cellexalObj@specie,  "is up to now not supported in the GO reports function" ))
	}
	if ( is.null( cellexalObj@usedObj$GO2genes)){
		cellexalObj@usedObj$GO2genes = AnnotationDbi::mapIds(x, AnnotationDbi::keys(x,'GO'), 'SYMBOL', 'GO', multiVals = 'list')
	}
	
	
	all = is.na(match(rownames(cellexalObj@data), genes ))
	names(all) = rownames(cellexalObj@data)
	all = factor(all)
	if ( length(table(all)) == 1) {
		message( "No genes of the list are in this object - This should not have happened!")
		return ( cellexalObj )
	}

	cellexalObj@usedObj$analysis = methods::new("topGOdata", ontology = ontology, allGenes=all
		,geneSel =  function(x) {x} ,  annot = topGO::annFUN.GO2genes, GO2genes= cellexalObj@usedObj$GO2genes)


	#resultFisher <- topGO::runTest(cellexalObj@usedObj$analysis, algorithm = "classic", statistic = "fisher")
	resultKS <- topGO::runTest(cellexalObj@usedObj$analysis, algorithm = "classic", statistic = "ks")
	resultKS.elim <- topGO::runTest(cellexalObj@usedObj$analysis, algorithm = "elim", statistic = "ks")
	topNodes <- as.numeric(topNodes)
	allRes <- topGO::GenTable(cellexalObj@usedObj$analysis,
			#classicFisher = resultFisher,
			classicKS = resultKS, elimKS = resultKS.elim,
			orderBy = "elimKS",  topNodes = topNodes)

	GOI_2_genes <- matrix( 1, nrow=topNodes, ncol=3)
	colnames(GOI_2_genes) = c("GO ID", "Rmd Gene list", "Mapping Gene List")
	for( i in 1:nrow(allRes) ) {
		GOI_2_genes[i,1] = allRes[i,1]
		GOI_2_genes[i,2] =  paste( intersect( genes,cellexalObj@usedObj$GO2genes[[allRes[i,1]]]), collapse=" ")
		GOI_2_genes[i,3] = paste(
				unlist( lapply(	intersect( genes,cellexalObj@usedObj$GO2genes[[allRes[i,1]]]),
		            rmdLink, link="https://www.genecards.org/cgi-bin/carddisp.pl?gene=", lineEnd=FALSE ))
			, collapse=" "
	    )
	}
	for ( i in 1:nrow(allRes) ) {
		allRes[i,1] = rmdLink(allRes[i,1],"http://amigo.geneontology.org/amigo/term/", lineEnd=FALSE ) #function definition in file 'rmdLink.R'
	}

	GOI_2_genes = cbind(GOI_2_genes,  allRes )	
	
	utils::write.table(GOI_2_genes, sep='\t', quote=F, row.names=F, file= 
					file.path( 
							cellexalObj@usedObj$sessionPath, 
							'tables', 
							filename( #function definition in file 'filename.R'
								c( 
									n,
									"GOgenes.csv"
								) 
							) 
					) 
	)
	
	GOI_2_genes = GOI_2_genes[,c(1,3)]
	
	allRes = allRes[,-c(4,5)] ## significant and expected columns do not contain info
	utils::write.table(allRes, sep='\t', quote=F, row.names=F, file= file.path( cellexalObj@usedObj$sessionPath, 'tables', 
					filename(c( n , "GOanalysis.csv") ) ) ) #function definition in file 'filename.R'
	## and now put this nice little table into the GEO section ;-)
	## and probably save this damn analysis object....

	if ( is.null( cellexalObj@usedObj$sessionPath)) {
		cellexalObj = sessionPath(cellexalObj) #function definition in file 'sessionPath.R'
	}
	sessionPath = cellexalObj@usedObj$sessionPath


	#mainOfile = file.path(sessionPath, filename( c( n, "GOanalysis.Rmd") )) #function definition in file 'filename.R'
	#fileConn<-file( mainOfile )
	#file.create(mainOfile)
	mainOfile = cellexalObj@usedObj$sessionRmdFiles[1]
	
	content = paste( sep="\n", 
					paste( "##", "GO analysis from Saved Selection", sessionCounter(  cellexalObj, cellexalObj@usedObj$lastGroup ) ), #function definition in file 'sessionCounter.R'
					paste("This selection is available in the R object as group", cellexalObj@usedObj$lastGroup ),
					"",
					paste( "### Genes"),
					paste( collapse=", ", unlist( lapply( genes,  rmdLink, link="https://www.genecards.org/cgi-bin/carddisp.pl?gene=" ))),
					"",
					paste( "The R package topGO was used to create this output table:"),
					" ",
					" ",
					paste( collapse="\n",knitr::kable(allRes, caption=paste("GO analysis for grouping", cellexalObj@usedObj$lastGroup ))),
					" ",
					paste( collapse="\n",knitr::kable(GOI_2_genes, caption=paste("The genes mapping to get GO ids" ))),
					"")

	cellexalObj = storeLogContents( cellexalObj, content, type="Ontology")
	id = length(cellexalObj@usedObj$sessionRmdFiles)
	cellexalObj = renderFile( cellexalObj, id, type="Ontology" )
	#close(fileConn)

	invisible(cellexalObj)

} )

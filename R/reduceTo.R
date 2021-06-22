#' reduceTo is the main cellexalvrR subsetting function. It can drop cells as well as genes
#' and makes sure, that all other objects still are in the same dimensianlity as the main data.
#' 
#' @name reduceTo
#' @docType methods
#' @description  The main reduction function can drop both samples and genes using the colnames/rownames of the data tables
#' @param x the cellexalvrR object
#' @param what reduce to samples or row ids default='row'
#' @param to select these names default=NULL
#' @title reduce a cellexalvrR object on either genes or columns
#' @export 
#if ( ! isGeneric('reduceTo') ){
setGeneric('reduceTo', ## Name
	function ( x, what='row', to=NULL ) { 
		standardGeneric('reduceTo') 
	}
)
#}


#' @rdname reduceTo
setMethod('reduceTo', signature = c ('cellexalvrR'),
	definition = function ( x, what='row', to=NULL ) {

			if (nrow(x@meta.gene) < nrow(x@data)) {
				x@meta.gene <- matrix(ncol=2, c(rownames(x@data), rep( 0, nrow(x@data)) ) )
				colnames(x@meta.gene) = c('Gene Symbol', 'useless')
				rownames(x@meta.gene) = rownames(x@data)
			}
			if ( ! is.null(to)) {
				if ( what =="row") {
					if ( length(which(is.na(match(to,rownames(x@data)))==F)) > 0 ) {
						useOnly <- match(to, rownames(x@data))
						not.matched <- to[is.na(useOnly)]
						if ( length(not.matched) > 0 ){
							print (paste('Problematic genes:', paste(not.matched,sep=', ')))
							to <- to[ ! is.na(useOnly)]
							useOnly <- useOnly[ ! is.na(useOnly) ]
						}
						x@data <- x@data[useOnly,]
						x@meta.gene <- x@meta.gene[useOnly,]
						for ( tName in names( x@usedObj$linearSelections)[-1]){
							if ( ! is.null( x@usedObj$linearSelections[[tName]]@geneClusters[['collapsedExp']] ) ) {
								x@usedObj$linearSelections[[tName]]@geneClusters[['collapsedExp']] = 
									x@usedObj$linearSelections[[tName]]@geneClusters[['collapsedExp']][useOnly,]
							}
						}
					}else {
						print (paste( "None of the probesets matched the probesets in the cellexalvr object -> keep everything!"))
					}
					
					
				}else if ( what =="col" ) {
					to <- tolower(to)
					if ( length(which(is.na(match(to,tolower(colnames(x@data))))==F)) > 0 ) {
						useOnly <- match(to, tolower(colnames(x@data)))
						not.matched <- to[is.na(useOnly)]
						if ( length(not.matched) > 0 ){
							print (paste('Problematic samples:', paste(not.matched,sep=', ')))
							to <- to[ ! is.na(useOnly)]
							useOnly <- useOnly[ ! is.na(useOnly) ]
						}
						n = ncol(x@data)
						x@data <- x@data[,useOnly]
						if ( nrow(x@meta.cell) == n ){
							x@meta.cell <- x@meta.cell[useOnly,]
						}
						if ( nrow(x@userGroups) == n){
							x@userGroups <- x@userGroups[useOnly,]
						}
						for ( na in names( x@drc) ) {
							## I need to cut them down, too
							## but they might have another dimension as the others!!
							if ( ! is.null(rownames(x@drc[[na]]))){
								here <- match(to, tolower(rownames(x@drc[[na]])))
								here = to[which(!is.na(here))]
								## existing IDs
								x@drc[[na]] = x@drc[[na]][match(here, tolower(rownames( x@drc[[na]]))),]
							}else {
								x@drc[[na]] = x@drc[[na]][useOnly,]
							}
							
						}
						for( na in names( x@groupSelectedFrom) ) {

							if ( class( x@groupSelectedFrom[[na]]) != 'cellexalGrouping'){
								x@groupSelectedFrom[[na]] = NULL
								next
							}
							if ( length( x@groupSelectedFrom[[na]]@order) == n){
								x@groupSelectedFrom[[na]]@order = x@groupSelectedFrom[[na]]@order[useOnly]
								x@groupSelectedFrom[[na]]@grouping = x@groupSelectedFrom[[na]]@grouping[useOnly]
							}
							
						}
						for( na in names(x@userGroups$linearSelections) ) {
							if ( length( which (is.na(match( rownames(x@userGroups$linearSelections[[na]]), colnames(x@data)) ))) > 0 ){
								x@userGroups$linearSelections[[na]] = subset( x@userGroups$linearSelections[[na]], colnames(x@data) )
							}
						}
												
					}else {
						print (paste( "None of the names (to) matched the sample names in the cellexalvr object -> keep everything!"))
					}
				}else {
					stop(paste( "the option what='",what,"' is not supported!", sep="") )
				}
			}
			invisible(x)
		}  )

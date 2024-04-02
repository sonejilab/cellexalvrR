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
setMethod('reduceTo', signature = c ('cellexalGrouping'),
	definition = function ( x, what='row', to=NULL ) {

		if (what == 'col' ){
			match_to_my_data = match( tolower(to), tolower(names(x)) )
		
			if (length(which(is.na(match_to_my_data))) == length(to) ) {
				return( NULL ) # this grouping does not define any cell any more
			}

			match_to_my_cells = match( tolower(to), tolower(names(x@grouping)) )

			if ( length(which(is.na(match_to_my_cells))) >0 ){
				if ( length( grep('pseudoBulk', x@gname)) > 0 ){
					return (NULL)
				}
				message( paste("retuning NULL as I (",x@gname,") never had info for these cells (n=",length(which(is.na(match_to_my_cells))),") like",
					paste( to[which(is.na(match_to_my_cells))][1:5] , collapse=", ", sep=", ")) )
				return (NULL)
			}
			tmp = rep(NA, length(x@grouping))
			names(tmp) = names(x@grouping)
			tmp[match_to_my_cells] = x@grouping[match_to_my_cells]

			x@grouping = factor(tmp[match_to_my_cells])

			tmp[match_to_my_cells] = x@VRgrouping[match_to_my_cells]
			x@VRgrouping = tmp[match_to_my_cells]

			tmp[match_to_my_cells] = x@order[match_to_my_cells]
			x@order = as.integer(tmp[match_to_my_cells])

			if ( ! empty(x@linarObj) ){
				x@linarObj = reduceTo( x@linarObj, what=what,to=to )
			}
		}
		invisible(x)
	}
)

#' @rdname reduceTo
setMethod('reduceTo', signature = c ('cellexalLinear'),
	definition = function ( x, what='row', to=NULL ) {
		if ( what  == "row" ){
			if (! is.null(x@geneClusters[['collapsedExp']]) ){
				x@geneClusters[['collapsedExp']] = x@geneClusters[['collapsedExp']][to,]
			}
		}
		if ( what == "col") {
			## reset the gene clusters
			x@geneClusters = list()
			x = subset(x, to)
			if ( is.null(x)){
				x=new('cellexalLinear')
			}
		}
		invisible (x)
	}
)
		



#' @rdname reduceTo
setMethod('reduceTo', signature = c ('cellexalvrR'),
	definition = function ( x, what='row', to=NULL ) {

			if (nrow(x@meta.gene) < nrow(x@data)) {
				x@meta.gene <- matrix(ncol=2, c(rownames(x@data), rep( 0, nrow(x@data)) ) )
				colnames(x@meta.gene) = c('Gene Symbol', 'useless')
				rownames(x@meta.gene) = rownames(x@data)
			}

			if ( length(x@usedObj$linearSelections) > 0 && ! is.null( x@usedObj$linearSelections )){
				x@usedObj$linearSelections = lapply( x@usedObj$linearSelections, reduceTo , what, to )
			}
			#x@userGroups$linearSelections = lapply( x@userGroups$linearSelections, reduceTo , what=what, to =to )
			# for( na in names(x@userGroups$linearSelections) ) {
			# 	x@userGroups$linearSelections[[na]] = reduceTo( x@userGroups$linearSelections[[na]],what=what, colnames(x@data) )
			# }

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
					}else {
						print (paste( "None of the probesets matched the probesets in the cellexalvr object -> keep everything!"))
					}
					
					
				}else if ( what =="col" ) {
					
					if ( length(which(is.na(match(to, colnames(x@data)))==F)) > 0 ) {
						useOnly <- match(to, colnames(x@data))
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
						if ( !is.null( x@usedObj$samples)) {
							x@usedObj$samples = x@usedObj$samples[useOnly,]
						}
						for ( na in names( x@drc) ) {
							## I need to cut them down, too
							## but they might have another dimension as the others!!
							if ( ! is.null(rownames(x@drc[[na]]))){
								here <- match(to, rownames(x@drc[[na]]))
								here = to[which(!is.na(here))]
								## existing IDs
								x@drc[[na]] = x@drc[[na]][match(here, rownames( x@drc[[na]])),]
							}else {
								if ( nrow( x@drc[[na]] ) > 0 ){ ## otherwise there is nothing to do here ;-)
									x@drc[[na]] = x@drc[[na]][useOnly,]
								}
							}
							
						}
						for( na in names( x@groupSelectedFrom) ) {
							if ( class( x@groupSelectedFrom[[na]]) != 'cellexalGrouping'){
								x@groupSelectedFrom[[na]] = NULL
								next
							}
							x@groupSelectedFrom[[na]] = reduceTo(x@groupSelectedFrom[[na]], what, to )					
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

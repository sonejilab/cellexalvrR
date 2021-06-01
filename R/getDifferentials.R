
#if ( ! isGeneric('renew') ){
setGeneric('getDifferentials', ## Name
			function (x,cellidfile,
					deg.method=c('wilcox', 'Seurat_wilcox', 'bimod', 'roc', 't', 'tobit', 'poisson', 'negbinom', 'MAST', 'DESeq2', 'anova'),
					num.sig=250, Log=TRUE, logfc.threshold = 1, minPct=0.1, onlyPos=TRUE) { 
				standardGeneric('getDifferentials') 
			}
	) 
#}

#' Identify differentially expressed genes.
#' 
#' This function makes three statistics available for the VR process 
#' (1) 'timeline' will automaticly be choosen it there is only one group in the data
#' (2) 'wilcox' a c++ re-implementation of the Seurat::FindAllMarkers function (default)
#' (3) 'Seurat_wilcox' the original Seurat::FindAllMarkers function (~10x slower than the c++ version and currently not working)
#' 
#' @name getDifferentials
#' @aliases getDifferentials,cellexalvrR-method
#' @rdname getDifferentials-methods
#' @docType methods
#' @description  Creates a heatmap from a selection of groups
#' The Seurat based statsictsics is applied only to genes expressed in at least 1 percent of the cells.
#' @param x, cellexalvrR object
#' @param cellidfile file containing cell IDs
#' @param deg.method The method to use to find DEGs ( 'wilcox', 'Seurat wilcox', 'bimod', 'roc', 't', 'tobit', 'poisson', 'negbinom', 'MAST', 'DESeq2' )
#' @param num.sig number of differnetial genes to return (250)
#' @param Log log the results (default=TRUE)
#' @param logfc.threshold the Seurat logfc.threshold option (default here 1 vs 0.25 in Seurat)
#' @param minPct the minium percent expressing cells in a group (default 0.1)
#' @param onlyPos select only genes showing an higher expression in the group (default =T)
#' @keywords DEGs
#' @title VR helper function getDifferentials
#' @examples 
#' \dontrun{
#' getDifferentials( x,  cellidfile= 'User.group.2', deg.method='wilcox')@usedObj$deg.genes #function definition in file 'getDifferentials.R'
#' }
#' @return the cellexalvrr object with the stats table stored in x@usedObj$sigGeneLists$Cpp[[x@usedObj$lastGroup]]
#' and significant genes can be accessed in the x@usedObj$deg.genes slot.
#' @export getDifferentials
setMethod('getDifferentials', signature = c ('cellexalvrR'),
		definition = function (x,cellidfile,
				deg.method=c('wilcox','Seurat_wilcox',  'bimod', 'roc', 't', 'tobit', 'poisson', 'negbinom', 'MAST', 'DESeq2', 'anova'),
				num.sig=250, Log=TRUE, logfc.threshold = 0.1, minPct=0.1, onlyPos=TRUE) {
			
			x <- loadObject(x) #function definition in file 'lockedSave.R'
			check(x)
			num.sig <- as.numeric( num.sig )
			
			accepted = c('wilcox','Seurat_wilcox',  'bimod', 'roc', 't', 'tobit', 'poisson', 'negbinom', 'MAST', 'DESeq2', 'anova')
			if ( sum(unlist(lapply( accepted, function(ok) { return ( ok == deg.method )} ))) != 1 ) {
				stop( paste('The deg.method',deg.method, 'is not supported' ) )
			}
			
			x <- userGrouping(x, cellidfile) #function definition in file 'userGrouping.R'
			
			ok <- which(!is.na(x@userGroups[,x@usedObj$lastGroup]))

			if ( length(ok) > 0) {
				loc <- reduceTo (x, what='col', to=colnames(x@data)[ ok ] ) #function definition in file 'reduceTo.R'
			}else {
				loc <- x
			}
			if ( ! is.na(match(paste(x@usedObj$lastGroup, 'order'), colnames(x@userGroups))) ){
				## at some time we had a problem in the creeation of order column names:
    			possible = c( paste(x@usedObj$lastGroup, c(' order','.order'), sep=""))
    			gname = possible[which(!is.na(match(possible, colnames(loc@userGroups))))]
				#browser()
				loc <- reorder.samples ( loc, gname ) #function definition in file 'reorder.obj.R'
			}
			
			info <- groupingInfo( loc ) #function definition in file 'groupingInfo.R'
			
			rem.ind <- which(Matrix::rowSums(loc@data)==0)
			
			grp.vec <- info$grouping
			
			col.tab <- info$col
			
			if(length(rem.ind)>0){
				loc = reduceTo(loc, what='row', to=rownames(loc@data)[-rem.ind]) #function definition in file 'reduceTo.R'
				grp.vec = grp.vec[-rem.ind]
			}
			
			deg.genes <- NULL
			if ( is.null(x@usedObj$sigGeneLists)) 
				x@usedObj$sigGeneLists = list()
			
			if(deg.method=='anova'){
				message('anova gene stats is deprecated - using wilcox instead!')
				deg.method= 'wilcox'
			}
			if(  length(table(info$grouping)) == 1 ){
				deg.method = 'Linear'
				stop( "Please selecting more than one group!")
				message('cor.stat linear gene stats timeline EXPERIMENTAL')
				if ( is.null( info$drc )) {
					message(paste("The linear stats has not gotten the drc information -- choosing the first possible" , names(loc@drc )[1] )) 
					info$drc = names(loc@drc )[1]
				}
				drc = loc@drc[[ info$drc ]]
				if ( is.null(drc) ){
					message(paste("the drc info",info$drc, "can not be found in the data! (", paste(collapse=", ", names(loc@drc)) ))
					message(paste("The linear stats has not gotten the drc information -- choosing the first possible" , names(loc@drc )[1] )) 
					info$drc = names(loc@drc )[1] ## for the log!
					drc = loc@drc[[ 1 ]]
				}

				#browser()
				if ( !is.null(rownames(drc))){
					OK = match( rownames(drc), colnames(x@data) )
				}else {
					OK = match( colnames(loc@data), colnames(x@data) )
				}
				a = drc[,1]

				if ( is.null(names(a))) {
					names(a) = colnames(loc@data)[OK]
				}
				loc = pseudotimeTest3D( loc, a, drc[,2], drc[,3], info$gname )

				message("After timeline calculation in smaller object:")
				check(loc)

				## so the new group needs to get into the main object:
				gname = loc@usedObj$lastGroup
				gnameO =  paste(sep=" ",gname , 'order')


				cellexalTime = loc@usedObj$timelines[[ gname ]] 
				#browser()
				x = addSelection( cellexalTime, x, info$gname)
				message("after time copy over:")
				check(x)
				#  rgl::plot3d( drc[OK,1], drc[OK,2], drc[OK,3], col=x@colors[[gname]][ x@userGroups[OK, gname ] ] )

				## run the correlation on a rolling window smothed information
				## It does not make sense to check genes that are hardly expressed at all in the group.
				## Lets say I want min 10% of the genes - how would that look?
				nCells = FastWilcoxTest::ColNotZero( Matrix::t( loc@data ) )
				OK = which( nCells / ncol(loc@data)  > .01 )

				loc = reduceTo(loc, what='row', to = rownames(loc@data)[OK]  )
				## this leads to horrible results
				if ( 1==0 ){
					nrol = round( length(OK) / 100 )
					if ( nrol < 10){
						nrol = 10
					}
					if ( nrol > 500){
						nrol = 500
					}	

					rolled <- FastWilcoxTest::rollSum( loc@data[, order(as.vector(loc@userGroups[, gname ] )) ], nrol )
					## create the stats
					ps <- FastWilcoxTest::CorNormalMatrix(  t(rolled), loc@userGroups[nrol:ncol(loc@data), gname ] ) 
				}
				ps = FastWilcoxTest::CorMatrix(  loc@data[, order(as.vector(loc@userGroups[, gname ] )) ], loc@userGroups[, gname ] ) 
				names(ps) = rownames(loc@data)

				ps[which(is.na(ps))] = 0
				o = order(abs( ps ), decreasing=TRUE)

				deg.genes = names(ps)[o[1:num.sig]]

				if ( is.null( x@usedObj$timelines)) {
					x@usedObj$timelines = list()
				}
				x@usedObj$timelines[['lastEntry']] = loc@usedObj$timelines[['lastEntry']]
				x@usedObj$timelines[[paste(info$gname, 'timeline')]] = loc@usedObj$timelines[['lastEntry']]
				
				## now we lack the heatmap here... But I would need one - crap!
				## create the smoothed data heatmap's

				colnames(p) = deg.genes
				hc = hclust( as.dist( 1- stats::cor(p, method='pearson') ) )
				deg.genes = hc$labels[hc$order]

				#ret = list( genes = split( names(gr), gr), ofile = ofile, pngs = pngs )
				ret = simplePlotHeatmaps( mat= p,  fname=file.path( x@usedObj$sessionPath,'png', gname ) )
				## add the plots to the log
				try( { 
					x = logTimeLine( x, ps, ret$genes, 
						groupingInfo( x,info$gname), png = c( ret$ofile, ret$pngs ), groupingInfo( x, gname ) ) 
				} )
				
				x@usedObj$sigGeneLists$lin[[x@usedObj$lastGroup]] = ps
				

				
			}else if ( deg.method == 'wilcox') {
				## use the faster Rcpp implementation
				CppStats <- function( n ) {
					OK = which(grp.vec == n )
					BAD= which(grp.vec != n )
					r = NULL
					if ( length(OK) > 0 & length(BAD) >0){
						try({
						r = as.data.frame(
								FastWilcoxTest::StatTest( Matrix::t( loc@data), OK, BAD, 
										logfc.threshold, minPct, onlyPos=onlyPos )
						)
						r= r[order( r[,'p.value']),]
						r = cbind( r, cluster= rep(n,nrow(r) ), gene=rownames(loc@data)[r[,1]] )
						})
					}
					r
				}
				
				all_markers = NULL;
				for ( n in  unique( sort(grp.vec)) ) {
					all_markers = rbind( all_markers, CppStats(n) )
				}
				
				#all_markers <- all_markers[ order( all_markers[,'p.value']),]
				if ( Log ) {
					try ( {logStatResult( x, 'Cpp', all_markers, 'p.value' ) }) #function definition in file 'logStatResult.R'
				}
				if ( is.null(x@usedObj$sigGeneLists$Cpp)) 
					x@usedObj$sigGeneLists$Cpp = list()
				x@usedObj$sigGeneLists$Cpp[[x@usedObj$lastGroup]] = all_markers
			}

			else {
				stop(paste('The stats method', deg.method, "is not supported by this version of cellexalvrR"))
			}
			
			### get the top genes
			if ( deg.method != 'Linear' ) {
				genes_list <- split( as.vector(all_markers[,'gene']), all_markers[,'cluster'] )
				ret_genes =  ceiling(num.sig / length(table(grp.vec)))
				
				if ( ret_genes < 1)
					ret_genes = 1
				
				top_genes <- function( x ) {
					if ( length(x) == 0) {
						NA
					}
					else if ( length(x) < ret_genes ) {
						x
					}else {
						x[1:ret_genes]
					}
				}
				
				## likely not the best approach..
				deg.genes = NULL
				ret_genes = ret_genes -1
				i = 0
				while ( length( deg.genes ) < num.sig ) {
					ret_genes = ret_genes +1
					i = i+1
					deg.genes = unique(unlist( lapply( genes_list,top_genes ) ))
					bad = which(is.na(deg.genes))
					if ( length(bad) > 0) 
						deg.genes = deg.genes[-bad]
					if ( i > 20)
						break
				}
				
				deg.genes = rownames(x@data)[ match( make.names(deg.genes), make.names( rownames( x@data) ) )]
				loc = reduceTo(loc, what='row', to=deg.genes) #function definition in file 'reduceTo.R'
				#tab <- as.matrix(Matrix::t(loc@data))
				if ( length(which(is.na( loc@userGroups[, loc@usedObj$lastGroup]) )) > 0 ) {
					## shit that will not work!
					loc = reduceTo(loc, what='col', to= which(is.na( loc@userGroups[, x@usedObj$lastGroup]) ==F) ) #function definition in file 'reduceTo.R'
				}
				
				tab <- t(FastWilcoxTest::collapse( loc@data, as.numeric(factor( as.vector(loc@userGroups[, loc@usedObj$lastGroup]) ) ), 1 )) ## simple sum up the data
				tab[which(tab == -Inf)] = 0
				hc <- stats::hclust(stats::as.dist( 1- stats::cor(tab, method='pearson') ),method = 'ward.D2')
				deg.genes = rownames(loc@data)[hc$order]
			}
			
			if ( length(deg.genes) == 0){
				message('deg.genes no entries - fix that')
				if ( interactive() ) {
					message ( 'no signififcant genes detected! - help needed: (exit with Q)' )
					browser()
				}else {
					message ( 'no signififcant genes detected!' )
				}
				
			}
			#promise <- future(lockedSave(x), evaluator = plan('multiprocess') ) #function definition in file 'lockedSave.R'
			## we only need to store the stats object here.
			## and as that is part of the usedObj we will store that ;-)
			## lockedSave(x) ## to much overheard! #function definition in file 'lockedSave.R'
			if ( ! interactive() ) { ## likely the VR scripts
				#print( paste('Do we reach this point?', 'usedObj', x@outpath ) )
				savePart( x, 'usedObj'); #function definition in file 'integrateParts.R'
				#print( 'And this - Do we reach this point, too?')
			}
			if ( length(deg.genes ) < 10){
				browser()
			}
			x@usedObj$deg.genes = deg.genes
			invisible( x )
		}
)


#' @describeIn getDifferentials cellexalvrR
#' @docType methods
#' @description preload the cellexalObj
#' @param x the cellexalvrR.RData file
#' @param cellidfile file containing cell IDs
#' @param deg.method The method to use to find DEGs ( 'wilcox', 'Seurat wilcox', 'bimod',
#' 'roc', 't', 'tobit', 'poisson', 'negbinom', 'MAST', 'DESeq2')
#' @param num.sig number of differnetial genes to return (250)
#' @param Log log the results (default=TRUE)
#' @param logfc.threshold the Seurat logfc.threshold option (default here 1 vs 0.25 in Seurat)
#' @param minPct the minium percent expressing cells in a group (default 0.1)
#' @keywords DEGs
#' @title description of function getDifferentials
#' @export getDifferentials
setMethod('getDifferentials', signature = c ('character'),
		definition = function (x,cellidfile,
				deg.method=c('wilcox', 'Seurat_wilcox', 'bimod', 'roc', 't', 'tobit', 'poisson', 'negbinom', 'MAST', 'DESeq2', 'anova'),
				num.sig=250, Log=TRUE, logfc.threshold = 1, minPct=0.1) {
			x <- loadObject(x) #function definition in file 'lockedSave.R'
			getDifferentials( x,cellidfile,deg.method,num.sig, Log=Log) #function definition in file 'getDifferentials.R'
		}
)

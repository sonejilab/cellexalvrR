#' Identify differentially expressed genes.
#' 
#' This function makes two statistics available for the VR process 
#' (1) 'linearSelection' will automaticly be choosen if there is only one group in the data
#' (2) 'wilcox' a c++ re-implementation of the Seurat::FindAllMarkers function (default)
#' 
#' @name getDifferentials
#' @docType methods
#' @description  Calculates the statistics for one CellexalVR selection file.
#' @param x, cellexalvrR object
#' @param cellidfile file containing cell IDs
#' @param deg.method The method to use to find DEGs (only 'wilcox' supported at the moment)
#' @param num.sig number of differnetial genes to return (250)
#' @param Log log the results (default=TRUE)
#' @param logfc.threshold the Seurat logfc.threshold option (default here 1 vs 0.25 in Seurat)
#' @param minPct the minium percent expressing cells in a group (default 0.1)
#' @param onlyPos select only genes showing an higher expression in the group (default =T)
#' @param report4genes a list of genes you want to get a report on.
#' @keywords DEGs
#' @title main Statistics function for cellexalvrR
#' @examples 
#' \dontrun{
#' getDifferentials( x,  cellidfile= 'User.group.2', deg.method='wilcox')@usedObj$deg.genes 
#' }
#' @return the cellexalvrr object with the stats table stored in x@usedObj$sigGeneLists$Cpp[[x@usedObj$lastGroup]]
#' and significant genes can be accessed in the x@usedObj$deg.genes slot.
#' @export 
#if ( ! isGeneric('getDifferentials') ){
setGeneric('getDifferentials', ## Name
			function (x,cellidfile,
					deg.method='wilcox',
					num.sig=250, Log=TRUE, logfc.threshold = 1, minPct=0.1, onlyPos=TRUE, report4genes= NULL ) { 
				standardGeneric('getDifferentials') 
			}
	) 
#}


#' @rdname getDifferentials
setMethod('getDifferentials', signature = c ('cellexalvrR'),
		definition = function (x,cellidfile,
				deg.method='wilcox',
				num.sig=250, Log=TRUE, logfc.threshold = 0.1, minPct=0.1, onlyPos=TRUE, report4genes= NULL ) {
			
			x <- loadObject(x) #function definition in file 'lockedSave.R'
			x= check(x)
			num.sig <- as.numeric( num.sig )
			
			accepted = c('wilcox','Seurat_wilcox',  'bimod', 'roc', 't', 'tobit', 'poisson', 'negbinom', 'MAST', 'DESeq2', 'anova')
			if ( sum(unlist(lapply( accepted, function(ok) { return ( ok == deg.method )} ))) != 1 ) {
				stop( paste('The deg.method',deg.method, 'is not supported' ) )
			}
			
			x <- userGrouping(x, cellidfile) #function definition in file 'userGrouping.R'
			cellidfile = x@usedObj$lastGroup

			ok <- which(!is.na(x@userGroups[,cellidfile]))

			if ( length(ok) > 0) {
				loc <- reduceTo (x, what='col', to=colnames(x@data)[ ok ] ) #function definition in file 'reduceTo.R'
			}else {
				loc <- x
			}
			if ( ! is.na(match(paste(cellidfile, 'order'), colnames(x@userGroups))) ){
				## at some time we had a problem in the creeation of order column names:
    			possible = c( paste(cellidfile, c(' order','.order'), sep=""))
    			gname = possible[which(!is.na(match(possible, colnames(loc@userGroups))))]
				#browser()
				loc <- reorderSamples ( loc, gname ) #function definition in file 'reorder.obj.R'
			}
			
			info <- groupingInfo( loc, cellidfile ) #function definition in file 'groupingInfo.R'
			
			rem.ind <- which(Matrix::rowSums(loc@data)==0)
			
			grp.vec <- info@grouping
			
			col.tab <- info@col
			
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
			if(  length(table(info@grouping)) == 1 ){
				deg.method = 'Linear'
				#stop( "Please selecting more than one group!")
				message('cor.stat linear gene stats linearSelection')
				if ( is.null( info@drc )) {
					message(paste("The linear stats has not gotten the drc information -- choosing the first possible" , names(loc@drc )[1] )) 
					info@drc = names(loc@drc )[1]
			
				}
				
				info = groupingInfo(x, info@gname ) ## get the info for the big object
				drc = x@drc[[ info@drc ]]
				 if ( is.null(drc) ){
				 	message(paste("the drc info",info@drc, "can not be found in the data! (", paste(collapse=", ", names(loc@drc)) ))
				 	message(paste("The linear stats has not gotten the drc information -- choosing the first possible" , names(loc@drc )[1] )) 
				 	info@drc = names(x@drc )[1] ## for the log!
				 	drc = x@drc[[ 1 ]]
				}
				
				cellexalLinear = NULL
				if (  nrow(info@linarObj@dat) == 0 ){
					message('defining time')
					x = pseudotimeTest3D( x, grouping= info@gname )
					info = groupingInfo( x, info@gname ) ## load changes
					cellexalLinear = info@linarObj
				}
				else {
					cellexalLinear = info@linarObj
				}
				message('creating reports')
				x  = createStats( cellexalLinear, x,  num.sig= num.sig )

				ret = createReport(cellexalLinear, 
					reduceTo(x, what='row', to = x@usedObj$deg.genes), 
					info = groupingInfo( x, info@gname ) 
				)
				timeN = cellexalLinear@gname
				x@usedObj$linearSelections[['lastEntry']] = x@usedObj$linearSelections[[timeN]] =
					 ret$cellexalObj@usedObj$linearSelections[[timeN]]
				
				deg.genes = x@usedObj$deg.genes

			}else if ( deg.method == 'wilcox') {
				## use the faster Rcpp implementation
				CppStats <- function( n ) {
					OK = which(grp.vec == n )
					BAD= which(grp.vec != n )
					r = NULL
					if ( length(OK) > 0 & length(BAD) >0){
						#try({
						r = as.data.frame(
								FastWilcoxTest::StatTest( Matrix::t( loc@data), OK, BAD, 
										logfc.threshold, minPct, onlyPos=onlyPos )
						)
						r= r[order( r[,'p.value']),]
						r = cbind( r, cluster= rep(n,nrow(r) ), gene=rownames(loc@data)[r[,1]] )
						#})
					}
					r
				}
				all_markers = NULL;
				for ( n in  unique( sort(grp.vec)) ) {
					all_markers = rbind( all_markers, CppStats(n) )
				}

				#all_markers <- all_markers[ order( all_markers[,'p.value']),]
				try ( {
					x = logStatResult( x,method='Cpp', data= all_markers, col='p.value' )
					## And get additional info about the genes


				}) #function definition in file 'logStatResult.R'
				
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

				bad = which( apply(tab, 2, var) == 0 )
				bad.genes = NULL
				if ( length(bad) > 0 ){
					tab = tab[, -bad]
					bad.genes = deg.genes[bad]
					deg.genes = deg.genes[-bad]
					loc =reduceTo( loc, what='row', to=deg.genes)
					message(paste(length(bad), "genes had a summary varianze of 0 in this comparison"))
				}
				hc <- stats::hclust(stats::as.dist( 1- stats::cor(tab, method='pearson') ),method = 'ward.D2')
				deg.genes = c(rownames(loc@data)[hc$order], bad.genes)
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
				if(interactive()) {
				 print("no deg genes - please check why!")
				 browser() 
				}
			}
			x@usedObj$deg.genes = deg.genes
			invisible( x )
		}
)


#' @rdname getDifferentials
setMethod('getDifferentials', signature = c ('character'),
		definition = function (x,cellidfile,
				deg.method='wilcox',
				num.sig=250, Log=TRUE, logfc.threshold = 1, minPct=0.1) {
			x <- loadObject(x) #function definition in file 'lockedSave.R'
			getDifferentials( x,cellidfile,deg.method,num.sig, Log=Log) #function definition in file 'getDifferentials.R'
		}
)

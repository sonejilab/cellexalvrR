
#' The log logics sometimes would benefit froma usable descriptions of the expression patterns.
#' Here we use the most simple heatmap functionallity in R (image) to show the expression values.
#' The returned genes list has the genes in the order of display in the heatmaps.
#'
#' In order to add usability the hetmap is split into an optimal number of sub heatmaps to show the trends.
#'
#' A new permanent variable in <cellexalvrR object>@userObj$gene_clusters (integer) can be used to override 
#' WSS based optimal grouping search.
#'
#' @name simplePlotHeatmaps
#' @docType methods
#' @description plot an extremely simple heatmap and slices of that
#' @param x, cellexalvrR object that has the correct order and only the genes of interest!
#' @param info the grouping info for this plot
#' @param fname the outfile base (.png for main .<i>.png for the slices)
#' @title create the heatmaps for the (interactive only) timline logs
#' @returns a list with the keys 
#'   png - all outfiles created; first summary others heatmaps),
#'   genes - the genes split into the displayed groups,
#'   ofile - the main outfile,
#'   error - any error occuring - should be included in the report,
#'   smoothedClusters - the smoothened lines,
#'	 MaxInCluster - which line has in which time slot it's maximum,
#'   mat - the z-scored summed up expression matrix.
#' @export
#if ( ! isGeneric('simplePlotHeatmaps') ){
setGeneric('simplePlotHeatmaps', ## Name
	function (x, info, fname ) { 
	#function ( mat, fname ) { 	
		standardGeneric('simplePlotHeatmaps')
	}
)
#}



#' @rdname simplePlotHeatmaps
setMethod('simplePlotHeatmaps', signature = c ('cellexalvrR', 'cellexalGrouping', 'character'),
	definition = function ( x, info, fname ) {
	#definition = function ( mat, fname ) {
	path = dirname(fname)
	if ( ! file.exists(path) ) {
		dir.create( path, recursive=TRUE)
	}


	## now I need to cellexalLinear object:
	ti = x@usedObj$linearSelections[[info@gname]]
	
	if ( is.null( ti ) ){
		## oops - we got a parentSelection?
		## best guess
		if ( x@usedObj$linearSelections[["lastEntry"]]@parentSelection == info@gname){
			ti = x@usedObj$linearSelections[["lastEntry"]]
			info = groupingInfo(  x, ti@gname)
		}
	}
	if ( is.null( ti ) ){
		stop(paste( "The time for the selection", info@gname, "could not be found") )
	}

	#print('simplePlotHeatmaps compact time Zscore')
	if ( ! is.null(x@usedObj$deg.genes) ) {
		toPlot = compactTimeZscore( ti, x@usedObj$deg.genes, x )
	}else {
		if ( nrow(x@data) > 3000) {
			stop("Huge dataset - Why are there no deg.genes!!")
		}
		toPlot = compactTimeZscore( ti, rownames(x@data), x )
	}

	error = NULL
	#print('simplePlotHeatmaps creating loes summary gene expressions')
	if ( nrow(toPlot) != 1000 ){
		rownames(toPlot) = rownames(ti@dat)
	}

	gr = clusterGenes( t(toPlot[, -c(1,2) ]), info = info, geneclusters = x@usedObj$gene_clusters  ) 
	clusterC = rainbow( max(gr$geneClusters) )
	pngs = NULL
	#create the separate simple Heatmap PNGs:
	ofile = paste( fname,'png', sep=".")
	#print ( "still OK 1")
	ma = -1000
	mi = 1000
	#print( "simplePlotHeatmaps plotting the heatmaps")
	message("creating report")
	smoothedClusters = list()
	for( i in 1:(length(gr)-2) ) {
		genes = names(gr$geneClusters)[which( gr$geneClusters == i)]
		gn = paste('gene.group',i, sep=".")
		smoothedClusters[[ gn ]] = gr[[i+1]]
		names(smoothedClusters[[ gn ]]) = rownames(toPlot)
		smoothedClusters[[ gn ]] = smoothedClusters[[ gn ]][which(! is.na(smoothedClusters[[ gn ]]))]
	}
	## could I use this here to create a heatmap with the cluster info
	grDevices::png(  paste(fname, "groupColors", 'png',sep="."), width=1000, height=300)
	graphics::image( matrix(as.numeric(info@linarObj@dat$col),ncol=1), col= levels(info@linarObj@dat$col))
	grDevices::dev.off()

	for( i in 1:(length(gr)-2)  ) {
		genes = names(gr$geneClusters)[which( gr$geneClusters == i)]
		gn = paste('gene.group',i, sep=".")	
		
		of = paste(fname, i, sep=".")

		of = plotTimeHeatmap( t(toPlot[,genes]), of,  color=clusterC[i], 
			circleF = paste(sep=".", ofile,i,'svg' ) )
		pngs = c(pngs, of)
	}

	plotDataOnTime ( data.frame(toPlot[,c('time', 'col')]), dat=smoothedClusters, ofile=ofile )
	#print("simplePlotHeatmaps finished")
	list( 
		genes = split( names(gr$geneClusters), gr$geneClusters), 
		ofile = ofile, 
		pngs = pngs, 
		groupColors =  paste(fname, "groupColors", 'png',sep="."),
		error= error,
		smoothedClusters = smoothedClusters,
		MaxInCluster = gr$MaxInCluster,
		mat=toPlot[,sort(names(gr$geneClusters))] 
	)

} )





#' Tries to answer the question: how do these genes differ over the linearSelection.
#' It answers this in a graphical, not a statistical way.
#' Hence you can feed whichever genelist you like into this function.
#'
#' @name clusterGenes
#' @aliases clusterGenes,cellexalLinear-method
#' @rdname clusterGenes
#' @docType methods
#' @description get a - hopefully - optimal grouing of a list of genes 
#' @param x either the z-scored matrix or a cellexalLinear object
#' @param deg.genes a list of genes 
#' @param info the group to cluster the genes for (list)
#' @param cellexalObj if x is a cellexalLinear object this is necessary to create the zscored matrix.
#' @param geneclusters ovverride the WSS based optimal group count search (default NULL)
#' @title cluster the genes for a linearSelection analysis
#' @export 
#if ( ! isGeneric('clusterGenes') ){
setGeneric('clusterGenes', ## Name
	function ( x, deg.genes=NULL, info=NULL,  cellexalObj= NULL, geneclusters=NULL ) { 
		standardGeneric('clusterGenes')
	}
)
#}



#' @rdname clusterGenes
setMethod('clusterGenes', signature = c ('cellexalLinear'),
	definition = function ( x, deg.genes=NULL, info=NULL, cellexalObj=NULL, geneclusters=NULL ) {

		if ( ! is.null(deg.genes)){
			cellexalObj = reduceTo( cellexalObj, what='rwo', to = deg.genes )
		}
		if ( ! is.null(info) ) {
			cellexalObj= reduceTo( cellexalObj, what='col', 
				colnames(cellexalObj@data)[which(! is.na( cellexalObj@userGroups[, info@gname]))] )
			cellexalObj = reorderSamples( cellexalObj, info@gname)
		}
		mat = FastWilcoxTest::ZScoreAll( x@data, display_progress=FALSE ) 
		colnames(mat) = colnames(x@dat)
		rownames(mat) = rownames(cellexalObj@data)

		clusterGenes( mat, deg.genes, info )
	}
)



#' @rdname clusterGenes
setMethod('clusterGenes', signature = c ('matrix'),
	definition = function ( x, deg.genes=NULL, info=NULL, cellexalObj=NULL, geneclusters=NULL ) {

		message("Clustering genes based on pca and kmeans")
		pca = irlba::prcomp_irlba ( x, center=T, n=3 )$x

		## determine how many groups we should split the genes into
		if ( !is.null(geneclusters)){
			optimum = geneclusters
		}
		else {
			points = unlist(lapply( 2:20, function(k) {  #total within-cluster sum of square (WSS)
			gr = stats::kmeans(pca,centers= k)$cluster
			names(gr) = rownames(x)
			#gr = cutree(hc, k);  
			mean( unlist( lapply( 1:k, function(id) {
				ret = 0
				if ( length( which(gr == id) ) > 1) {
					dat = x[which(gr == id), ]
					ret = (ncol(dat)-1)*sum(apply(dat,1,var) )
				}
				ret
				} ) ), na.rm=TRUE
			)
			} ) )
			## create a linear function between start: 1;points[1] and end: length(points);points[length(points)]
			slope <- diff(c(points[1], points[length(points)] ))/diff(c(1,length(points)))
			intercept <- points[1]-slope
			f = function(x) { x * slope + intercept }
			der = unlist(lapply( 1:length(points) ,function(x) { points[x] - f(x) }))
			der = der- min(der)
			## And find the max length of this value
			## here more groups is likely better than less
			optimum <- max ( which(der < max(der) / 1e+10) )
			## plot( points, der)
			## abline( v= points[optimum], col='red')
			## this should not be standard, but lets just get a little more than that. Better more than too little info.
			optimum = optimum + 1 # one more is most of the time really better!
			## if more are whished for use the x@usedObj$gene_clusters value instead.
			## now we lack the heatmap here... But I would need one - crap!
			## add a simple one - the most simple one ever, but use a subcluster of genes, too!!
		}
		if (is.null(geneclusters)){
			geneclusters = "'NULL'"
		}
		#message(paste( "Gene grouping is going for", optimum, "clusters and asked for is", geneclusters) )
		gn = stats::kmeans(pca,centers= optimum)$cluster
		names(gn) = rownames(x)
		geneTrajectories = list(MaxInCluster = list())
		
		if ( ! is.null(info@linarObj ) ) {
			cT = collapseTime( info@linarObj )
			m = match( colnames(x), rownames(cT@dat))
			if ( length(is.na(m)) == 1000 ) {
				colnames(x) = rownames(cT@dat)
				m = match( colnames(x), rownames(cT@dat))
			}

			defaultW <- getOption("warn")
			options(warn = -1)
			for( i in unique(gn) ) {
				genes = names(gn)[which(gn == i)]
				groupname = paste("P",i, sep="")
				## should not be necessary, but sometimes it is:
				#print(i)

				geneTrajectories[[groupname]] = tryCatch( { 
					stats::predict( stats::loess( apply (x[genes,], 2, mean) ~ cT@dat[m,'time'], span=.005) )
				}, error=function(err){ 
					message("loess failed with span .005")
					} )
				
				if ( length(which(is.na(geneTrajectories[[groupname]]))) == 0 ){
					tryCatch({
						geneTrajectories[[groupname]] = stats::predict( stats::loess( apply (x[genes,], 2, mean) ~ cT@dat[m,'time'], span=.2) )
						}, error=function(err){message("loess failed with span .2") })	
				}

				inClusters = sapply( split( geneTrajectories[[groupname]], cT@dat$col[m]), max, na.rm=T )

				geneTrajectories[['MaxInCluster']][[groupname]] = 
					c( which( inClusters ==max(inClusters)[1]), max(inClusters)[1] )
			}
			options(warn = defaultW)
		}
		df = t(data.frame(geneTrajectories$MaxInCluster))
		new_order = rownames(df[order( df[,1], -df[,2]),])
		new_order = as.numeric(unlist(stringr::str_replace_all(new_order, 'P', '')))
		tmp = as.vector( gn )

		ret = list(MaxInCluster = list())
		for ( i in 1:length(new_order) ){
			groupname =paste(sep="", 'G',i)
			oldGN = paste(sep="", 'P',new_order[i])
			gn[which(tmp == new_order[i])] = i
			ret[[groupname]] = as.vector(geneTrajectories[[oldGN]])
			names(ret[[groupname]]) = names(geneTrajectories[[oldGN]])
			ret$MaxInCluster[[groupname]] = geneTrajectories$MaxInCluster[[oldGN]]
		}
		ret[['geneClusters']] = gn
		## now we need to order the gropoups by there highest value in a area.
		#print ( "clusterGenes finished")
		ret
	}
)

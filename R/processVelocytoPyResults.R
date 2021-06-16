# #' Velocytopy is becoming more and more important and the analysis (normally)
# #' creates a normalized expression hdf5 file and a more detailed (e.g. variable genes based)
# #' dimensional reduction hdf5 file.
# #' 
# #' The processed file likely also has velocity information which can also be added 
# #' in the correct way to the cellexalvrR object.
# #' 
# #' For the VR process bothe the embedings as well as (all) expression data
# #' is of interest.
# #' 
# #' Here both files are processed and the resulting 
# #' @name processVelocytoPyResults
# #' @aliases processVelocytoPyResults,cellexalvrR-method
# #' @rdname processVelocytoPyResults
# #' @docType methods
# #' @description Combine two anndata files into one cellexalvrR object
# #' @param total the hdf5 file containing all expression values (normalized)
# #' @param variable  the hdf5 file containing the embeddings and velocyto data
# #' @param embeddings the embedding names default= c('umap', 'phate')
# #' @param embeddingDims the dimensionality of the embeddings (default 3)
# #' @param velocyto should velocyto data be added to the embeddings default=TRUE
# #' @param veloScale velocyto scaling factor default=20
# #' @param specie the species the data has been created from default='human'
# #' @param minCell4gene the total file might contain genes not expressed at all - filter them default= 10
# #' @title description of function processVelocytoPyResults
# #' @export 
# setGeneric('processVelocytoPyResults', ## Name
# 	function (total, variable, embeddings= c('umap', 'phate'), embeddingDims=3, velocyto =TRUE, veloScale=20, specie='human', minCell4gene = 10) { 
# 		standardGeneric('processVelocytoPyResults')
# 	}
# ) 


# setMethod('processVelocytoPyResults', signature = c ('character'),
# 	definition = function (total, variable, embeddings= c('umap', 'phate'), embeddingDims=3, velocyto =TRUE, veloScale=20, specie='human', minCell4gene = 10) {
# 	if (!require("hdf5r", quietly = TRUE ) == T ) {
# 		stop("package 'hdf5r' needed for this function to work. Please install it.",
# 				call. = FALSE)
# 	}
# 	if ( ! hdf5r::is_hdf5(variable)) {
# 		stop( "The variable genes / analyzed VelocytoPy outfile if no h5ad file")
# 	}
# 	if ( ! hdf5r::is_hdf5(total)) {
# 		stop( "The total genes VelocytoPy outfile if no h5ad file")
# 	}
# 	file <- H5File$new(variable, mode='r')
# 	## parse the data into a sparse matrix
# 	toSparse <- function(file){
# 		message("reading expression data")
# 		x= file[['X']][['data']][]
# 		i= file[['X']][['indices']][]
# 		j= rep(0, length(x))
# 		indptr = file[['X']][['indptr']][]
# 		last = 1
# 		for ( a in 2: length(indptr) ) {
# 			j[(indptr[a-1]+1):(indptr[a]+1)] = last
# 			last = last+1
# 		}
# 		j = j [-length(j)]
# 		m = Matrix::sparseMatrix( i = i+1, j=j, x=x)
# 		meta.data = H5Anno2df( file, 'obs')
# 		annotation = H5Anno2df( file,'var')
		
# 		rownames(m) = annotation[,'_index']
# 		colnames(m) = meta.data[,'_index']
		
# 		m
# 	}
# 	m = toSparse( file )
# 	meta.data = H5Anno2df( file, 'obs')
# 	annotation = H5Anno2df( file, 'var')
	
# 	drcs = lapply(embeddings, function(n) {  
# 				ret = t(file[['obsm']][[paste(sep="_",'X',n)]][1:embeddingDims,])
# 				if ( embeddingDims == 2 ){
# 					ret = cbind(ret, rep(0, nrow(ret)) )
# 				}
# 				ret
# 			} )
# 	names(drcs) = embeddings
# 	cellexalvrR = new( 'cellexalvrR', 
# 			data=m, meta.cell=as.matrix(meta.data), 
# 			meta.gene=as.matrix(annotation), 
# 			drc = drcs, specie = specie )
	
# 	if ( velocyto ) {
# 		for ( n in names(cellexalvrR@drc)) {
# 			velo_n = paste( sep="_", 'velocity', n )
# 			cellexalvrR@drc[[n]] = 
# 				cbind( 
# 					cellexalvrR@drc[[n]], 
# 					cellexalvrR@drc[[n]][,1:embeddingDims] + t(file[['obsm']][[velo_n]][,] * veloScale)
# 				)
# 			if ( embeddingDims == 2 ){
# 				cellexalvrR@drc[[n]] =
# 					cbind(cellexalvrR@drc[[n]],rep(0, nrow(cellexalvrR@drc[[n]])))
# 			}
# 		}
# 	}
	
# 	file2 <- H5File$new(total, mode='r')
	
# 	m = toSparse( file2 )
# 	annotation = H5Anno2df( file2, 'var')
	
# 	if ( ncol(m) != ncol(cellexalvrR@data)){
# 		stop( paste("the variable data has",ncol(cellexalvrR@data),"cells and the total",ncol(m),"mismatch not allowd!" ))
# 	}
# 	annotation$varGene = rep(0, nrow(annotation))
# 	annotation$varGene[match(rownames(cellexalvrR@data), rownames(m))] = 1
	
# 	## possible, that the more analyzed data has dropped some cells
# 	if ( ! ( all.equal( colnames(cellexalvrR@data), colnames(m)) == TRUE ) ) {
# 		OK_cells <- match(colnames(cellexalvrR@data), colnames(m))
# 		if ( length(which(is.na(OK_cells))) > 0 ) {
# 			cellexalvrR = reduceTo( cellexalvrR, what='col', to = colnames(cellexalvrR@data)[which(! is.na(OK_cells))])
# 		}
# 	}
	
# 	## and filter the low expression gene, too
# 	rsum = Matrix::rowSums( m )
# 	OK_genes = which(rsum >= minCell4gene)
# 	mOK = m[OK_genes,]
	
# 	#cellexalvrR@meta.gene= matrix()
# 	cellexalvrR@data = mOK
# 	cellexalvrR@meta.gene = as.matrix(annotation[OK_genes,])
# 	cellexalvrR
# } )

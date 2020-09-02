#' This function is part of the conversion path of a BioData::R6 object 
#' into a ret object.
#'
#' The function will most likely not be of importance to anybody but me.
#' @name as_cellexalvrR
#' @aliases as_cellexalvrR,environment-method
#' @rdname as_cellexalvrR-methods
#' @docType methods
#' @description convert a BioData list (BioData library not loaded) into a ret obejct
#' @param x the BioData 'object'
#' @param meta.cell.groups which x$samples column to convert to meta.cell classes
#' @param meta.genes.groups which annotation columns to keep (default NULL)
#' @param userGroups which x$samples columns to add to the userGroups slot
#' @param outpath set the outpath of the object (default getwd())
#' @param specie set the specie to either mouse or human (default check gene names)
#' @title convert a BioData object to ret keeping all 3D drc objects.
#' @export 
setGeneric('as_cellexalvrR', ## Name
	function ( x, meta.cell.groups=NULL, meta.genes.groups = NULL, userGroups=NULL, outpath=getwd(), specie, ... ) { 
		standardGeneric('as_cellexalvrR')
	}
)

setMethod('as_cellexalvrR', signature = c ('environment'),
	definition = function ( x, meta.cell.groups=NULL, meta.genes.groups = NULL, userGroups=NULL, outpath=getwd(), specie ) {
	## x has to be a BioData object which is read as a simple list here!
	if ( is.null(meta.cell.groups)){
		browser()
		stop( paste(sep="","meta.cell.groups is not defined - please set it to one of\n'", 
			paste(collapse="', '", colnames(x$samples)) ,"'") )
	}
	ret = methods::new('cellexalvrR')
	ret@data = x$zscored
	#ret@data@x = log( exp( ret@data@x ) +1 ) ## fixed in BioData
	
	if ( ! is.null(meta.genes.groups) )
		ret@meta.gene = x$annoatation[, meta.genes.groups]
	ret@meta.cell = make.cell.meta.from.df( x$samples[,meta.cell.groups] ,rq.fields= meta.cell.groups ) #function definition in file 'make.cell.meta.from.df.R'
	rownames(ret@meta.cell) = colnames( ret@data )
	t = data.frame(lapply( 
		x$usedObj$userGroups, 
		function(n) {
			OK = which(! is.na( x$samples[,n]))
			order=as.vector(x$samples[,n])
			order[OK] = 1:length(OK)
			list( x$samples[,n],order) 
		} ))
	
	colnames(t) = unlist(lapply( x$usedObj$userGroups, function (n) paste( n, c("", "order"))))
	ret@userGroups = t
	
	DRC <- names(x$usedObj)[grep ( 'MDS', names(x$usedObj))]
	OK = grep ( '_dim_' , DRC, invert= TRUE )
	if ( length(OK) == 0 ) {
		stop( "ret does need at least one 3D DRC structure to work on - please create that first!")
	}
	for ( n in DRC[OK] ) {
		for ( n2 in names(x$usedObj[[n]]) ) {
			new_name = stringr::str_replace_all( n2, "\\s", "_")
			ret@drc[[new_name]] = x$usedObj[[n]][[n2]]
		}
	}
	ret@colors = x$usedObj$colorRange
	ret@specie=x$usedObj$specie
	
	bad = which( ret@data@x < 0)
	if ( length(bad) > 0 ) {
		ret@data@x[ bad ] = 0
		ret@data = Matrix::drop0(ret@data)
	}
	ret@outpath = outpath
	ret
} )


setMethod('as_cellexalvrR', signature = c ('Seurat'),
	definition = function ( x, meta.cell.groups=NULL, meta.genes.groups = NULL, userGroups=NULL, 
		outpath=getwd(), specie, assay=NULL, slot='data', velocity='none', scale.arrow.travel=20 ) {

		ret = methods::new('cellexalvrR')
		getEmb = function (n, cn) {
			emb = Embeddings(object = x, reduction = n)
			 if ( ncol(emb) ==2){
			 	emb = cbind(emb, rep(0,nrow(emb)))
			 }
			 if ( ncol(emb) > 3) {
			 	emb = emb[,1:3]
			 }
			 rownames(emb) = cn
			 emb
		}
		if ( is.null( meta.cell.groups ) ) {
			stop(paste( sep="", 
				"The 'meta.cell.groups' is undefined - you need to select values from:\n'",
				paste( colnames(x@meta.data), collapse="', '"), "'") )
		}
		if ( .hasSlot(x, 'data')) {
			warning( "Untested Seurat object version 2.x")
			ret@data = x@data
			ret@drc = lapply( names(x@dr), getEmb, colnames(ret@data) )
			names(ret@drc) = names(x@dr)
		}
		else {
			ret@data = GetAssayData(object = x, assay = assay, slot = slot )
			ret@drc = lapply( names(x@reductions), getEmb, colnames(ret@data)  )
			names(ret@drc) = names(x@reductions)
		}
		if ( velocity == 'scvelo') {

			drc = list()
			for ( n in names(ret@drc)) {
				if ( length(grep('^velocity_', n)) == 1 ){
					n_source = stringr::str_replace( n, '^velocity_', '') 
					drc[[n_source]] = cbind(ret@drc[[n_source]], ret@drc[[n_source]] + ret@drc[[n]] * scale.arrow.travel)
				}

			}
			ret@drc = drc
		}
		ret@meta.cell = make.cell.meta.from.df( x@meta.data, meta.cell.groups)
		rownames(ret@meta.cell) = colnames(ret@data)
		ret@meta.gene = matrix( ncol=1,  rownames(ret@data) )
		colnames(ret@meta.gene) = "Gene Symbol"
		ret@specie = specie
		ret
	})




setMethod('as_cellexalvrR', signature = c ('character'),
	definition = function (x, meta.cell.groups=NULL, meta.genes.groups = NULL, userGroups=NULL, outpath=getwd(), 
		specie, velocity='scvelo', scale.arrow.travel=20 ){
		# , embeddings = c('umap', 'phate'), embeddingDims=3, velocyto =TRUE, veloScale=20, minCell4gene = 10

		## re-write of this function:
		## Use Seurat-Data instead...
		ok = TRUE
		if (!require("Seurat", quietly = TRUE ) == T ) {
			ok =FALSE
			warning("package 'Seurat' needed for this function to work. Please install it.",
				call. = FALSE)
		}
		if (!require("SeuratDisk", quietly = TRUE ) == T ) {
			ok =FALSE
			warning("package 'SeuratDisk' needed for this function to work. Please install it.",
				call. = FALSE)
		}
		ret = NULL
		if ( ok ) {
			ifile = stringr::str_replace( x, 'h5ad$', 'h5seurat')
			if ( ! file.exists( ifile )){
				Convert(x, dest = "h5seurat", overwrite = TRUE)
			}
			ifile = stringr::str_replace( x, 'h5ad$', 'h5seurat')
			seurat <- LoadH5Seurat( ifile )
			ret = as_cellexalvrR(seurat, meta.cell.groups, meta.genes.groups =meta.genes.groups, outpath= outpath, specie=specie, 
				velocity='scvelo', scale.arrow.travel=20 )
			## to not screw up the database!
			ret@data = ret@data [which(Matrix::rowSums(ret@data) > 0),]
		}else {
			if (!require("hdf5r", quietly = TRUE ) == T ) {
				stop("fall back package 'hdf5r' needed for this function to work. Please install it.",
				call. = FALSE)
			}
			file = H5File$new(x, mode='r')
			ret = as_cellexalvrR(file, meta.cell.groups, meta.genes.groups, userGroups, outpath, 
				specie, velocyto = velocity == 'scvelo', veloScale = scale.arrow.travel, minCell4gene = 1 )
		}
		ret
} )

#setMethod('as_cellexalvrR', signature = c ('character'),
#	definition = function (x, meta.cell.groups=NULL, meta.genes.groups = NULL, userGroups=NULL, outpath=getwd(), 
#		specie, embeddings = c('umap', 'phate'), embeddingDims=3, velocyto =TRUE, veloScale=20, minCell4gene = 10 ){
#
#	if (!require("hdf5r", quietly = TRUE ) == T ) {
#		stop("package 'hdf5r' needed for this function to work. Please install it.",
#				call. = FALSE)
#	}
#	if ( ! hdf5r::is_hdf5(x)) {
#		stop( "The variable genes / analyzed VelocytoPy outfile if no h5ad file")
#	}
#	file <- H5File$new(x, mode='r')
#
#	as_cellexalvrR(file, meta.cell.groups, meta.genes.groups, userGroups, outpath, 
#		specie,  embeddings , embeddingDims, velocyto, veloScale, minCell4gene  )
#
#})


setMethod('as_cellexalvrR', signature = c ('H5File'),
	definition = function (x,  meta.cell.groups=NULL, meta.genes.groups = NULL, userGroups=NULL, outpath=getwd(),
	 specie, embeddings = c('umap', 'phate'), embeddingDims=3, velocity =TRUE, veloScale=20, minCell4gene = 10) {

	if (!require("hdf5r", quietly = TRUE ) == T ) {
		stop("package 'hdf5r' needed for this function to work. Please install it.",
				call. = FALSE)
	}
		if ( length(embeddings) == 0 ) {
			stop("A CellexalVR session without 3D embeddings is not making sense! STOP.")
		}
	## parse the data into a sparse matrix
	toSparse <- function(file){
		message("reading expression data")
		x= file[['X']][['data']][]
		i= file[['X']][['indices']][]
		j= rep(0, length(x))
		indptr = file[['X']][['indptr']][]
		last = 1
		for ( a in 2: length(indptr) ) {
			j[(indptr[a-1]+1):(indptr[a]+1)] = last
			last = last+1
		}
		j = j [-length(j)]
		m = Matrix::sparseMatrix( i = i+1, j=j, x=x)

		meta.data = H5Anno2df( file, 'obs')
		annotation = H5Anno2df( file,'var')
		
		rownames(m) = annotation[,'_index']
		colnames(m) = meta.data[,'_index']
		
		m
	}
	m = toSparse( x )
	meta.data = H5Anno2df( x, 'obs')
	annotation = H5Anno2df( x, 'var')

	#browser()
	drcs = lapply(embeddings, function(n) {  
				ret = t(x[['obsm']][[paste(sep="_",'X',n)]][1:embeddingDims,])
				if ( embeddingDims == 2 ){
					ret = cbind(ret, rep(0, nrow(ret)) )
				}
				ret
			} )
	names(drcs) = embeddings
	ret = new( 'cellexalvrR', 
			data=m, meta.cell=as.matrix(meta.data), 
			meta.gene=as.matrix(annotation), 
			drc = drcs, specie = specie )
	

	if ( velocity ) {
		for ( n in names(ret@drc)) {
			velo_n = paste( sep="_", 'velocity', n )
			ret@drc[[n]] = 
				cbind( 
					ret@drc[[n]], 
					ret@drc[[n]][,1:embeddingDims] + t(x[['obsm']][[velo_n]][,] * veloScale)
				)
			if ( embeddingDims == 2 ){
				ret@drc[[n]] =
					cbind(ret@drc[[n]],rep(0, nrow(ret@drc[[n]])))
			}
		}
	}
	
	for ( n in names( ret@drc) ) {
		rownames(ret@drc[[n]]) = colnames(ret@data)
	}
	## and filter the low expression gene, too
	rsum = FastWilcoxTest::ColNotZero( Matrix::t(m) )
	OK_genes = which(rsum >= minCell4gene)
	mOK = m[OK_genes,]
	
	#ret@meta.gene= matrix()
	ret@data = mOK
	ret@meta.gene = as.matrix(annotation[OK_genes,])
	ret@specie = specie
	ret
} )

#' @name forceAbsoluteUniqueSample
#' @aliases forceAbsoluteUniqueSample,ret-method
#' @rdname forceAbsoluteUniqueSample-methods
#' @docType methods
#' @description  This function adds _<id> to all duplicate values thereby enforcing uniques.
#' @param x the string vector you want to force into uniques
#' @param separator the separator between orig str and id ( default '_')
#' @title description of function forceAbsoluteUniqueSample
#' @export 
setGeneric('forceAbsoluteUniqueSample', ## Name
	function ( x ,separator='_') { ## Argumente der generischen Funktion
		standardGeneric('forceAbsoluteUniqueSample') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('forceAbsoluteUniqueSample', signature = c ('ret'),
	definition = function ( x ,separator='_') {
	ret <- vector(length=length(x))
	last = x[1]
	ret[1] <- last
	for ( i in 2:length(x) ){
		last = x[i]
		if ( ! is.na(match( last, ret )) ){
			last <- paste(last,separator,sum( ! is.na(match( x[1:i], last )))-1, sep = '')
		}
		ret[i] <- last
	}
	ret
} )


#' @name H5Anno2df
#' @aliases H5Anno2df,ret-method
#' @rdname H5Anno2df-methods
#' @docType methods
#' @description  convert a H5 annotation (any name) table to a data table
#' @param x the H5 object
#' @param slotName the H5 entity tro convert to a data.frame
#' @param namecol the (optional) rownames column for the data
#' @param onlyStrings return only columns that not only contain numbers (default FALSE)
#' @title description of function H5Anno2df
#' @export 
#setGeneric('H5Anno2df', ## Name
#	function (x, slotName, namecol=NULL, onlyStrings=FALSE ) { ## Argumente der generischen Funktion
#		standardGeneric('H5Anno2df') ## der Aufruf von standardGeneric sorgt für das Dispatching
#	}
#)#

#setMethod('H5Anno2df', signature = c ('H5File'),#
	#definition = function (x, slotName, namecol=NULL, onlyStrings=FALSE ) {
#  		obs = data.frame(lapply(names(x[[slotName]]), function(n) { x[[paste(sep="/",slotName,n)]][] } ))
 # 		colnames( obs ) = names(x[[slotName]])
#  		col_uniq= NULL
#  		for( n in colnames(obs) ) {#
#	  		if ( all(obs[,n] =="") ){
#  				obs[,n] = NULL
#  			}else {
#  				col_uniq = c( col_uniq, length(unique(obs[,n]))) 
#  			}
#  		}
#  		names(col_uniq) = colnames( obs )
#  		## most likely cell names column
#  		if ( ! is.na(match(namecol, colnames(obs)) )) {#
#			rownames(obs) =  forceAbsoluteUniqueSample ( #function definition in file 'as_cellexalvrR.R'
#				as.vector(obs[, namecol]) )
# 		}else {
#  			## now I need to check for strings...
#  			OK = unlist(lapply( colnames(obs) , function(id) {
#  				a= which( is.na(as.numeric(as.vector(obs[,id])))==T) ## Strings only
#  				if ( length(a) > 0) {
#  					length(unique(as.vector(obs[a, id])))
#  				}else {
#  						0
#  				}
#  			}))
#  			names(OK) = colnames(obs)
#  			# if ( slotName == 'row_attrs'){
#  			# 	browser()
#  			# }
#  			rownames(obs) =  forceAbsoluteUniqueSample ( #function definition in file 'as_cellexalvrR.R'
#  				as.vector(obs[, names(OK)[which(OK == max(OK))[1]]]) )
#  		}
#  		if ( onlyStrings ) {
#  			for( i in 1:length(col_uniq) ) {
#  				if ( col_uniq[i] == 0 ){ # all entries convertable to numeric
#  					obs[,i] = NULL
#  				}
#  			}
#  		}
# })

#' Here two Velocyto file results (actually andata hdf5 files) 
#' one containing the var gene analysis with a meaningful clustering
#' and one with all genes. This function will combine the drc models from the var gene analysis 
#' and it's clustering with the gene expressions in the bigger file.
#' 
#' The output will be a cellexalvrR object. 
#' @name H5Anno2df
#' @aliases H5Anno2df,cellexalvrR-method
#' @rdname H5Anno2df-methods
#' @docType methods
#' @description  convert a H5 annotation (any name) table to a data table
#' @param x the H5 object
#' @param slotName the H5 entity tro convert to a data.frame
#' @param namecol the (optional) rownames column for the data
#' @param onlyStrings return only columns that not only contain numbers (default FALSE)
#' @title description of function H5Anno2df
#' @export 
setGeneric('H5Anno2df', ## Name
		function (x, slotName, namecol=NULL, onlyStrings=FALSE ) { ## Argumente der generischen Funktion
			standardGeneric('H5Anno2df') ## der Aufruf von standardGeneric sorgt für das Dispatching
		}
)

setMethod('H5Anno2df', signature = c ('H5File'),
		definition = function (x, slotName, namecol=NULL, onlyStrings=FALSE ) {
			OK = NULL;
			for (n in names( x[[slotName]])) {
				if ( is(x[[paste(sep="/",slotName,n)]], 'H5Group') ){
					OK= c( OK, FALSE )
				}else {
					OK = c(OK, TRUE)
				}
			}
			obs = data.frame(lapply(names(x[[slotName]])[OK], function(n) { x[[paste(sep="/",slotName,n)]][] } ))
			colnames( obs ) = names(x[[slotName]])[OK]
			col_uniq= NULL
			for( n in colnames(obs) ) {
				if ( all(obs[,n] =="") ){
					obs[,n] = NULL
				}else {
					col_uniq = c( col_uniq, length(unique(obs[,n]))) 
				}
			}
			names(col_uniq) = colnames( obs )
			## most likely cell names column
			if ( ! is.null(namecol )) {
				rownames(obs) =  forceAbsoluteUniqueSample ( #function definition in file 'as_cellexalvrR.R'
						as.vector(obs[, namecol]) )
			}else {
				## now I need to check for strings...
				OK = unlist(lapply( colnames(obs) , function(id) {
									a= which( is.na(as.numeric(as.vector(obs[,id])))==T) ## Strings only
									if ( length(a) > 0) {
										length(unique(as.vector(obs[a, id])))
									}else {
										0
									}
								}))
				names(OK) = colnames(obs)
				# if ( slotName == 'row_attrs'){
				# 	browser()
				# }
				rownames(obs) =  make.names ( #function definition in file 'as_cellexalvrR.R'
						as.vector(obs[, names(OK)[which(OK == max(OK))[1]]]) )
			}
			if ( onlyStrings ) {
				for( i in 1:length(col_uniq) ) {
					if ( col_uniq[i] == 0 ){ # all entries convertable to numeric
						obs[,i] = NULL
					}
				}
			}
			
			obs
		} )



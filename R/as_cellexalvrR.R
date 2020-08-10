#' This function is part of the conversion path of a BioData::R6 object 
#' into a cellexalvrR object.
#'
#' The function will most likely not be of importance to anybody but me.
#' @name as_cellexalvrR
#' @aliases as_cellexalvrR,environment-method
#' @rdname as_cellexalvrR-methods
#' @docType methods
#' @description convert a BioData list (BioData library not loaded) into a cellexalvrR obejct
#' @param x the BioData 'object'
#' @param meta.cell.groups which x$samples column to convert to meta.cell classes
#' @param meta.genes.groups which annotation columns to keep (default NULL)
#' @param userGroups which x$samples columns to add to the userGroups slot
#' @param outpath set the outpath of the object (default getwd())
#' @param specie set the specie to either mouse or human (default check gene names)
#' @title convert a BioData object to cellexalvrR keeping all 3D drc objects.
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
		stop( "cellexalvrR does need at least one 3D DRC structure to work on - please create that first!")
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
	definition = function ( x, meta.cell.groups=NULL, meta.genes.groups = NULL, userGroups=NULL, outpath=getwd(), specie, assay=NULL ) {

		ret = methods::new('cellexalvrR')
		getEmb = function (n) {
			emb = Embeddings(object = x, reduction = n)
			 if ( ncol(emb) ==2){
			 	emb = cbind(emb, rep(0,nrow(emb)))
			 }
			 if ( ncol(emb) > 3) {
			 	emb = emb[,1:3]
			 }
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
			ret@drc = lapply( names(x@dr), getEmb )
			names(ret@drc) = names(x@dr)
		}
		else {
			ret@data = GetAssayData(object = x, assay = assay )
			ret@drc = lapply( names(x@reductions), getEmb  )
			names(ret@drc) = names(x@reductions)
		}
		ret@meta.cell = make.cell.meta.from.df( x@meta.data, meta.cell.groups)
		ret@meta.gene = matrix( ncol=1,  rownames(ret@data) )
		colnames(ret@meta.gene) = "Gene Symbol"

		ret
	})

# setMethod('as_cellexalvrR', signature = c ('character'),
# 	definition = function ( x, meta.cell.groups=NULL, meta.genes.groups = NULL, userGroups=NULL, outpath=getwd(), specie ) {
	
# 	if (!file.exists(x)) {
# 		stop("sorry, x is not file I can read from")
# 	}
	
# 	if ( length(grep('.h5ad$',x)) ==1 ) {
# 		x= hdf5r::h5file(x)
# 		as_cellexalvrR( x, meta.cell.groups=meta.cell.groups,  #function definition in file 'as_cellexalvrR.R'
# 			meta.genes.groups = meta.genes.groups, userGroups= userGroups, outpath=outpath, 
# 			specie = specie)
# 	}else if( length(grep('.loom$',x)) ==1 ) {
# 		x <- loomR::connect(filename = x, mode = "r")
# 		as_cellexalvrR( x, meta.cell.groups=meta.cell.groups,  #function definition in file 'as_cellexalvrR.R'
# 			meta.genes.groups = meta.genes.groups, userGroups= userGroups, outpath=outpath, 
# 			specie = specie)
# 	}
# 	else {
# 		stop(paste("Sorry the file",x,"is not supported by this function" ) )
# 	}
# } )

# #' Cast a loom object to a cellexalvrR object
# setMethod('as_cellexalvrR', signature = c ('loom'),
# 	definition = function ( x, meta.cell.groups=NULL, meta.genes.groups = NULL, userGroups=NULL, outpath=getwd(), specie ) {
# 		## code adapted from Seurat::ReadH5AD.H5File() 2019/08/22
	
# 	require( 'hdf5r' )

# 	ret = methods::new('cellexalvrR')
# 	ret@specie = specie

# 	as.sparse <- function(x, ...) {
#   for (i in c('data', 'indices', 'indptr')) {
#     if (!x$exists(name = i) || !is(object = x[[i]], class2 = 'H5D')) {
#       stop("Invalid H5Group specification for a sparse matrix, missing dataset ", i)
#     }
#   }
#   if ('h5sparse_shape' %in% hdf5r::h5attr_names(x = x)) {
#     return(Matrix::sparseMatrix(
#       i = x[['indices']][] + 1,
#       p = x[['indptr']][],
#       x = x[['data']][],
#       dims = rev(x = hdf5r::h5attr(x = x, which = 'h5sparse_shape'))
#     ))
#   }
#   return(Matrix::sparseMatrix(
#     i = x[['indices']][] + 1,
#     p = x[['indptr']][],
#     x = x[['data']][],
#     sparse=T
#   ))
# 	}
# 	print ( "reading cell information" )
# 	ret@usedObj$original_meta.cell = obs = H5Anno2df(x,'col_attrs', 'cell_names', onlyStrings=TRUE ) #function definition in file 'as_cellexalvrR.R'
#   	## now we check which ones the user wanted and throw an error if we did not get anything
#   	if ( is.null(meta.cell.groups)){
#   		cat( paste(
#   			"meta.cell.groups is missing", 
#   			"Please select some from the list:", 
#   			paste( sep="","c('", paste(colnames(obs), collapse="', '" ),"')"),sep='\n','' ) )
#   		stop("Please give me a meta.cell.groups vector!")
#   	}else {
#   		if ( length( which(is.na( match(meta.cell.groups, colnames(obs)) )) ) > 0 ){
#   			bad = which(is.na( match(meta.cell.groups, colnames(obs) )))
#   			cat( paste( sep="\n", 
#   				"I could not find the sample columns", 
#   				paste(collapse=", ",meta.cell.groups[bad]),
#   				"But I have these:",
#   				 paste( sep="", "c('",paste( colnames(obs), collapse="', '" ),"')")
#   				,''))
#   			stop("please select existsing cell annotation columns!")
#   		}
#   		if ( length(meta.cell.groups) == 1){
#   			obs = matrix(obs[, meta.cell.groups], ncol=length(meta.cell.groups))
#   		} else {
#   			obs =obs[, meta.cell.groups]
#   		}
#   	}
#   	# col_complexity = apply( obs, 2, function(x) { length( unique( as.vector(x) ) ) })
#   	# names( col_complexity) = colnames(obs)
#   	# obs = matrix(obs)
#   	# ## Get rid of all columns that are too complex for cellexalVR
#   	# names( col_complexity) = colnames( obs )
#   	# if ( length(which( col_complexity > 50)) > 0 ){
#   	# 	if ( length(which( col_complexity < 51)) <2 ) {
#   	# 		## problems with the matrix reduced to a vector/list
#   	# 		tmp = obs
#   	# 		obs = matrix(obs[, - which( col_complexity > 50)])
#   	# 		rownames( obs) = rownames(obs)
#   	# 	}else {
#   	# 		obs = obs[, - which( col_complexity > 50)]
#   	# 	}
#   	# }
#   	# for ( i in length(col_complexity):1){
#   	# 	if (col_complexity[i] > 50 ){
#   	# 		obs = obs[ ,-i]
#  		# }
#   	# }
#   	print ( "reading data" )
#   	if (is(object = x[['matrix']], class2 = 'H5Group')) {
#     	dat <- as.sparse(x = x[['matrix']])
#   	} else {
#    		dat <- x[['matrix']][, ]
#   	}
#   	# x will be an S3 matrix if X was scaled, otherwise will be a dgCMatrix
#   	if (is.matrix(x = dat)) {
#   		## the loom files seam to store all data as matrix and not as sparse matrix?!
#   		dat = Matrix::Matrix(dat, sparse=T)
#   	}
#   	print ( "reading feature data")
#   	ret@usedObj$original_meta.features = meta.features = H5Anno2df(x, 'row_attrs', 'gene_names', onlyStrings=TRUE ) #function definition in file 'as_cellexalvrR.R'

#   	dat = Matrix::t(dat)
#   	rownames(dat) = rownames( meta.features)
#   	colnames(dat) = rownames(obs)
# 	ret@data = dat
# 	ret = addCellMeta2cellexalvr(ret, makeCellMetaFromDataframe(obs, rq.fields= colnames(obs))) #function definition in file 'addElements.R'
# 	ret@meta.gene = as.matrix(meta.features[match( rownames(ret@data), rownames(meta.features) ),])

# 	rm( dat)
# 	rm( meta.features)
# 	rm(obs)

# 	## The drc's are hidden in the obj
# 	interest <- list( 
# 		'unknown' = c('_X', '_Y', '_Z'), 
# 		'tSNE' = c('_tSNE1', '_tSNE2', '_tSNE3'), 
# 		'PCA' = c('_PC1', '_PC2', '_PC3') 
# 	)
# 	print ("reading drc data")
# 	dr = lapply( interest, function(a) { 
# 		d=NULL
		
# 		if ( var(ret@usedObj$original_meta.cell[,a[1]]) + var (ret@usedObj$original_meta.cell[,a[2]]) != 0 ){

# 			## the third column is not defined in the loom structures. Hence I simply do not check for it here
# 			d= cbind(as.numeric(as.vector(ret@usedObj$original_meta.cell[,a[1]])), as.numeric(as.vector(ret@usedObj$original_meta.cell[,a[2]])), rep(0,nrow(ret@meta.cell)) )
# 			colnames(d) = a
# 			rownames(d) = rownames(ret@meta.cell)
# 		}
# 		d
# 		} )

# 	for( n in names(dr) ) {
# 		if ( ! is.null(dr[[n]])) {
# 			ret@drc[[n]] = dr[[n]]
# 		}
# 	}


#     if ( length(names(ret@drc)) == 0) {
#     	stop( "No usable drc's found in the loomR object")
#     }

#     ret@specie = specie
#     ret@outpath = outpath
#     #print ( "Please take care colnames and rownames have not been set!" )
#     invisible(ret)
# } )

# #' Cast an AnnData object to a cellexalvrR object
# setMethod('as_cellexalvrR', signature = c ('H5File'),
# 	definition = function ( x, meta.cell.groups=NULL, meta.genes.groups = NULL, userGroups=NULL, outpath=getwd(), specie ) {
# 		## code adapted from Seurat::ReadH5AD.H5File() 2019/08/22
	
# 	require( 'hdf5r' )

# 	ret = methods::new('cellexalvrR')
# 	ret@specie = specie

# 	as.sparse <- function(x, ...) {
#   for (i in c('data', 'indices', 'indptr')) {
#     if (!x$exists(name = i) || !is(object = x[[i]], class2 = 'H5D')) {
#       stop("Invalid H5Group specification for a sparse matrix, missing dataset ", i)
#     }
#   }
#   if ('h5sparse_shape' %in% hdf5r::h5attr_names(x = x)) {
#     return(sparseMatrix(
#       i = x[['indices']][] + 1,
#       p = x[['indptr']][],
#       x = x[['data']][],
#       dims = rev(x = hdf5r::h5attr(x = x, which = 'h5sparse_shape'))
#     ))
#   }
#   return(sparseMatrix(
#     i = x[['indices']][] + 1,
#     p = x[['indptr']][],
#     x = x[['data']][]
#   ))
# 	}
#   	if (is(object = x[['X']], class2 = 'H5Group')) {
#     	dat <- as.sparse(x = x[['X']])
#   	} else {
#    		dat <- x[['X']][, ]
#   	}
#   	# x will be an S3 matrix if X was scaled, otherwise will be a dgCMatrix
#   	print ( "accessing expression data")
#   	scaled <- is.matrix(x = dat)

#   	print ( "accessing cell data")
# 	ret@usedObj$original_meta.cell <- obs <- x[['obs']][]

# 	print ( "accessing feature data")
#   	dat.var <- x[['var']][]
#   	#browser()
#   	rownames(x = dat) = rownames(x = dat.var) = dat.var$index
#   	colnames(x = dat) = rownames(x = obs) = obs$index
#   	## col_complexity - we can not handle really complex information bits
#   	## some 50 colors is the maximum na dwould blow up the data table quite significantly.
#   	## hen ce we remove all columns with too complex information - if asked for
#   	if ( is.null(meta.cell.groups)){
#   		cat( paste(
#   			"meta.cell.groups is missing", 
#   			"Please select some from the list:", 
#   			paste( sep="","c('", paste(colnames(obs), collapse="', '" ),"')"),sep='\n','' ) )
#   		stop("Please give me a meta.cell.groups vector!")
#   	}else {
#   		if ( length( which(is.na( match(meta.cell.groups, colnames(obs)) )) ) > 0 ){
#   			bad = which(is.na( match(meta.cell.groups, colnames(obs) )))
#   			cat( paste( sep="\n", 
#   				"I could not find the sample columns", 
#   				paste(collapse=", ",meta.cell.groups[bad]),
#   				"But I have these:",
#   				 paste( sep="", "c('",paste( colnames(obs), collapse="', '" ),"')")
#   				,''))
#   			stop("please select existsing cell annotation columns!")
#   		}
#   		if ( length(meta.cell.groups) == 1){
#   			obs = data.frame(obs[, meta.cell.groups])
#   			colnames(obs) = meta.cell.groups
#   		} else {
#   			obs =obs[, meta.cell.groups]
#   		}
#   	}
  	

#   	x.slot = FALSE
#   	meta.features <- NULL
#   	## here we do not handle anything in cellexalVR and hence can keep everything.
#   	if ( scaled ){
#   		if (x$exists(name = 'raw.X')) {
#   			dat <- as.sparse(x = x[['raw.X']])
#     		add.var <- x[['raw.var']][]
#     		slot(object = dat, name = 'Dim') <- c(nrow(x = add.var), nrow(x = obs))
    		
#     		rownames(x = dat) <- rownames(x = add.var) <- add.var$index
#     		colnames(x = dat) <- rownames(obs)
#     		add.var <- add.var[, -which(x = colnames(x = add.var) == 'index'), drop = FALSE]
#     		#Merging feature-level metadata dataframes
#     		dat.var <- dat.var[, -which(x = colnames(x = dat.var) %in% colnames(x = add.var))]
#    			meta.features <- merge(x = add.var, y = dat.var, by = 0, all = TRUE)
#     		rownames(x = meta.features) <- meta.features$Row.names
#     		meta.features <- meta.features[, -which(x = colnames(x = meta.features) == 'Row.names'), drop = FALSE]
#     		rm(add.var)
#     		rm ( dat.var)
#   		}else {
#   			stop( "The AnnData object does not contain unscaled data!")
#   		}
#   	}else {
#   		meta.features <- dat.var
#   		rm ( dat.var )
#   	}

# 	## obtain the data slot information
# 	ret@data = dat
# #	browser()
# 	ret = addCellMeta2cellexalvr(ret, make.cell.meta.from.df(data.frame(obs), rq.fields= colnames(obs))) #function definition in file 'addElements.R'
# 	ret@meta.gene = as.matrix(meta.features[match( rownames(ret@data), rownames(meta.features) ),])

# 	rm( dat )
# 	rm( meta.features)
# 	rm(obs)

# 	if ( ! x$exists(name = 'obsm') ) {
# 		stop( "The AnnData does not contain any dimension reduction datasets needed for cellexalVR" )
# 	}
# 	# Pull cell embeddings
# 	embed.reduc <- x[['obsm']]$get_type()$get_cpd_labels()
# 	embed.n <- sapply(
#       X = x[['obsm']]$get_type()$describe()$cpd_types,
#       FUN = '[[',
#       'array_dims'
#     )
#     names(x = embed.n) <- embed.reduc
#     ncells <- x[['obsm']]$dims
#     embeddings <- lapply(
#       X = embed.reduc,
#       FUN = function(r) {
#         return(t(x = vapply(
#           X = 1:ncells,
#           FUN = function(i) {
#             return(x[['obsm']][i][[r]])
#           },
#           FUN.VALUE = numeric(length = embed.n[[r]])
#         )))
#       }
#     )
#     names(x = embeddings) <- embed.reduc
#     for (i in 1:length(x = embeddings)) {
#       rownames(x = embeddings[[i]]) <- colnames(x = ret@data )
#     }
#     ## now add all >= 3D MDS objects to ret
#     for ( name in names(embeddings)) {
#     	if ( ncol(embeddings[[name]] ) >= 3 ){
#     		## get rid of all dimensions > 3
#     		ret@drc[[name]] = embeddings[[name]][,1:3] 
#     	}else {
#     		ret@drc[[name]] = cbind( embeddings[[name]], z= rep(0,nrow(embeddings[[name]])) )
#     	}
#     }
#     if ( length(names(ret@drc)) == 0) {
#     	stop( "No usable drc's found in the AnnData object")
#     }

#     ret@specie = specie
#     ret@outpath = outpath

#     invisible(ret)
# } )


#' @name forceAbsoluteUniqueSample
#' @aliases forceAbsoluteUniqueSample,cellexalvrR-method
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

setMethod('forceAbsoluteUniqueSample', signature = c ('cellexalvrR'),
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

setMethod('H5Anno2df', signature = c ('cellexalvrR'),
	definition = function (x, slotName, namecol=NULL, onlyStrings=FALSE ) {
  		obs = data.frame(lapply(names(x[[slotName]]), function(n) { x[[paste(sep="/",slotName,n)]][] } ))
  		colnames( obs ) = names(x[[slotName]])
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
  		if ( ! is.na(match(namecol, colnames(obs)) )) {
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
  			rownames(obs) =  forceAbsoluteUniqueSample ( #function definition in file 'as_cellexalvrR.R'
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

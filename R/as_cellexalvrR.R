#' This function is the default conversion function.
#' Depending on which object is to be imported the paramteters differ.
#'
#' @name as_cellexalvrR
#' @docType methods
#' @description convert a supported data set into a cellexalvrR obejct
#' @param x a Seurat object or scanpy/scvelo h5ad file
#' @param meta.cell.groups which cell annotation columns to convert to meta.cell
#' @param meta.genes.groups which annotation columns to keep (default NULL)
#' @param userGroups which cell annotation columns to add to the userGroups slot
#' @param outpath set the outpath of the object (default getwd())
#' @param specie set the specie to either mouse or human (default check gene names)
#' @param ... allow additional data type specififc parameters 
#' @title convert a supported object/file to cellexalvrR keeping all 3D drc objects.
#' @export 
setGeneric('as_cellexalvrR', ## Name
	function ( x, meta.cell.groups=NULL, meta.genes.groups = NULL, userGroups=NULL, 
		outpath=getwd(), specie, ... ) { 
		standardGeneric('as_cellexalvrR')
	}
)


#' @rdname as_cellexalvrR
setMethod('as_cellexalvrR', signature = c ('environment'),
	definition = function ( x, meta.cell.groups=NULL, meta.genes.groups = NULL, 
		userGroups=NULL, outpath=getwd(), specie ) {
	## x has to be a BioData object which is read as a simple list here!
	if ( is.null(meta.cell.groups)){
		if(interactive()) { browser() }
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



#' @rdname as_cellexalvrR
#' @param assay Seurat::GetAssayData parameter 'assay' to fetch the expression data (default NULL)
setMethod('as_cellexalvrR', signature = c ('Seurat'),
	definition = function ( x, meta.cell.groups=NULL, meta.genes.groups = NULL,
	 userGroups=NULL, outpath=getwd(), specie, assay=NULL ) {

		ret = methods::new('cellexalvrR')
		getEmb = function (n) {
			emb = Seurat::Embeddings(object = x, reduction = n)
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
			ret@drc = lapply( names(x@dr), function(drc){ 
				drc = getEmb(drc)
				if ( ncol(drc) ==2){
					drc = cbind( drc, rep(0, nrow(drc)))
				}else if (  ncol(drc) > 3 ) {
					drc = drc[,1:3]
				}
				drc
				}
			)
			names(ret@drc) = names(x@dr)
		}
		else {
			ret@data = Seurat::GetAssayData(object = x, assay = assay )
			ret@drc = lapply( names(x@reductions), getEmb  )
			names(ret@drc) = names(x@reductions)
		}
		ret@meta.cell = make.cell.meta.from.df( x@meta.data, meta.cell.groups)
		ret@meta.gene = matrix( ncol=1,  rownames(ret@data) )
		colnames(ret@meta.gene) = "Gene Symbol"

		ret
	})





#' @rdname as_cellexalvrR
#' @param embeddings which embeddings to import from the file (default NULL = all)
#' @param embeddingDims the dimensionality of the embeddings (default 3)
#' @param velocity import velocity information (default = 'scvelo')
#' @param scaleArrowTravel scale the velovity arrow for VR (default 20)
#' @param minCell4gene savety feature for andata import required for CellexalVR (default 10)
setMethod('as_cellexalvrR', signature = c ('character'),
	definition = function (x,   meta.cell.groups=NULL, meta.genes.groups = NULL, userGroups=NULL, outpath=getwd(), 
		specie, embeddings = NULL, embeddingDims=3, velocity ='scvelo', scaleArrowTravel=20, minCell4gene = 10 ){

	if (!require("hdf5r", quietly = TRUE ) == T ) {
		stop("package 'hdf5r' needed for this function to work. Please install it.",
				call. = FALSE)
	}
	if ( ! hdf5r::is_hdf5(x)) {
		stop( "The variable genes / analyzed VelocytoPy outfile is no .h5ad file")
	}
	file <- hdf5r::H5File$new(x, mode='r')

	as_cellexalvrR(file, meta.cell.groups, meta.genes.groups, userGroups, outpath, 
		specie,  embeddings , embeddingDims, velocity, scaleArrowTravel, minCell4gene  )

})



#' @rdname as_cellexalvrR
setMethod('as_cellexalvrR', signature = c ('H5File'),
	definition = function (x,  meta.cell.groups=NULL, meta.genes.groups = NULL, userGroups=NULL, outpath=getwd(),
	 specie, embeddings = NULL, embeddingDims=3, velocity ='scvelo', scaleArrowTravel=20, minCell4gene=10) {

		
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
		if ( is.na(match("_index", colnames(annotation))) ){
			rownames(m) = rownames(annotation)
			colnames(m) = rownames(meta.data)
			annotation[,'_index'] = rownames(m)
			meta.data[,'_index']  = colnames(m)
		}else{
			rownames(m) = annotation[,'_index']
			colnames(m) = meta.data[,'_index']
		}

		m
	}

	m = NULL
	m = tryCatch( { toSparse( x )},
		error = function(err) {
			if ( ! is.na(match('read', names(x[['X']])))) {
				#warning( "using method 2 to read the data")
				m = x[['X']][['read']]()
				m = Matrix( m, sparse=TRUE)
				meta.data = H5Anno2df( x, 'obs')
				annotation = H5Anno2df( x,'var')
				if ( is.na(match("_index", colnames(annotation))) ){
					rownames(m) = rownames(annotation)
					colnames(m) = rownames(meta.data)
					annotation[,'_index'] = rownames(m)
					meta.data[,'_index']  = colnames(m)
				}else{
					rownames(m) = annotation[,'_index']
					colnames(m) = meta.data[,'_index']
				}
			}
			m = m
		}
	)
	if ( is.null(m) ) {
		stop( "I could not obtain the expression data from the h5 file")
	}
	meta.data = H5Anno2df( x, 'obs')
	annotation = H5Anno2df( x, 'var')
	if ( is.na(match("_index", colnames(annotation))) ){
		annotation[,'_index'] = rownames(m)
		meta.data[,'_index']  = colnames(m)
	}
	
	if ( is.null(embeddings) ) {
		## extract all if embeddings == NULL
		embeddings = names(x[['obsm']])[grep( '^X_', names(x[['obsm']]))]
		embeddings = unlist(stringr::str_replace_all(embeddings, '^X_', '' ))
	}
	if ( length(embeddings) == 0 ) {
			message("A CellexalVR session without 3D embeddings is not making sense! STOP?!")
	}

	drcs = lapply(embeddings, function(n) {  
				ret = t(x[['obsm']][[paste(sep="_",'X',n)]][1:embeddingDims,])
				if ( embeddingDims == 2 ){
					ret = cbind(ret, rep(0, nrow(ret)) )
				}
				rownames(ret) = meta.data[,'_index']
				ret
			} )

	names(drcs) = embeddings

	cellexalvrR = new( 'cellexalvrR', 
			data=m, meta.cell=as.matrix(meta.data), 
			meta.gene=as.matrix(annotation), 
			drc = drcs, specie = specie )
	
	## save the original information
	cellexalvrR@usedObj$samples = meta.data

	if ( is.null(velocity) ) {
		## that is OK - obviousel no veloctiy information to be imported
	}
	else if ( velocity == 'scvelo' ) {
		for ( n in names(cellexalvrR@drc)) {
			velo_n = paste( sep="_", 'velocity', n )
			tryCatch({
				cellexalvrR@drc[[n]] = 
					cbind( 
						cellexalvrR@drc[[n]], 
						cellexalvrR@drc[[n]][,1:embeddingDims] + t(x[['obsm']][[velo_n]][,] * scaleArrowTravel)
					)
				if ( embeddingDims == 2 ){
					cellexalvrR@drc[[n]] =
						cbind(cellexalvrR@drc[[n]],rep(0, nrow(cellexalvrR@drc[[n]])))
				}
			},
			error = function(e) { 
				message( paste( "WARNING: drc", n, "- velocity information not available") )
			}
			)
		}
	}else {
		message( paste( "WARNING: velocity information can only imported for 'scvelo'") )
	}
	
	## and filter the low expression gene, too

	rsum = FastWilcoxTest::ColNotZero( Matrix::t(m) )
	OK_genes = which(rsum >= minCell4gene)
	mOK = m[OK_genes,]
	
	#cellexalvrR@meta.gene= matrix()
	cellexalvrR@data = mOK
	cellexalvrR@meta.gene = as.matrix(annotation[OK_genes,])

	if ( !is.null(meta.cell.groups)){
		cellexalvrR@meta.cell = make.cell.meta.from.df( cellexalvrR@usedObj$samples, meta.cell.groups )
	}else {
		warning( "Please convert the meta.cell data into the requred 0/1 table by using the make.cell.meta.from.df function before exporting theis to VR." )
	}

	if ( ! all.equal(colnames(cellexalvrR@data), rownames(cellexalvrR@meta.cell)) == TRUE ){
		if ( all.equal(make.names(colnames(cellexalvrR@data)), rownames(cellexalvrR@meta.cell)) == TRUE ){
			rownames(cellexalvrR@meta.cell) = colnames(cellexalvrR@data)
		}
	}
	if ( ! all(rownames(cellexalvrR@data) == rownames(cellexalvrR@meta.gene))){
		rownames(cellexalvrR@meta.gene) = rownames(cellexalvrR@data)
	}

	cellexalvrR
} )


#' @name forceAbsoluteUniqueSample
#' @docType methods
#' @description  This function adds _<id> to all duplicate values thereby enforcing uniques.
#' @param x the string vector you want to force into uniques
#' @param separator the separator between orig str and id ( default '_')
#' @title make the cell names unique
#' @export 
setGeneric('forceAbsoluteUniqueSample', ## Name
	function ( x ,separator='_') { ## Argumente der generischen Funktion
		standardGeneric('forceAbsoluteUniqueSample') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

#' @rdname forceAbsoluteUniqueSample
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


#' Convert e.g. a h5ad 'obs' object into a R::data.frame.
#' @name H5Anno2df
#' @docType methods
#' @description  convert a H5 annotation (any name) table to a data table
#' @param x the H5File object
#' @param slotName the H5 slot(s) to convert to a data.frame
#' @param namecol the (optional) rownames column for the data
#' @param onlyStrings return only columns that not only contain numbers (default FALSE)
#' @title convert a H5 annotation (any name) table to a data table
#' @export 
setGeneric('H5Anno2df', ## Name
		function (x, slotName, namecol=NULL, onlyStrings=FALSE ) { ## Argumente der generischen Funktion
			standardGeneric('H5Anno2df') ## der Aufruf von standardGeneric sorgt für das Dispatching
		}
)


#' @rdname H5Anno2df
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
					suppressWarnings({a= which( is.na(as.numeric(as.vector(obs[,id])))==T) })
					## Strings only
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
#' This function converts the cellexal object to a minimal Seurat object.
#' It is meant to export the cellsexal object for usage with ShinyCell or
#' similar packages mainly to improve the log functionality.
#'
#' @name as_Seurat
#' @docType methods
#' @description convert a supported data set into a Seurat object
#' @param x a cellexalvrR object 
#' @title convert a supported object/file to cellexalvrR keeping all 3D drc objects.
#' @returns a Seurat object with the data from the cellexal object.
#' @export 
setGeneric('as_Seurat', ## Name
	function ( x ) { 
		standardGeneric('as_Seurat')
	}
)


#' @rdname as_Seurat
setMethod('as_Seurat', signature = c ('cellexalvrR'),
	definition = function ( x ){
		test = CreateSeuratObject( x@data, project="CellexalvrR_TestData", min.cells = 3, min.features=200 )
		if ( !is.null(x@usedObj$samples)){
			test@meta.data = x@usedObj$samples
		}
		for (name in x@drc){
			test[[ name ]] = CreateDimReducObject(embeddings = x@drc[[name]], key = paste(name,"_",sep=""), assay = 'RNA')
		}
		test
	}
)


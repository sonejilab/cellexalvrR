
#' This function adds a 0/1 information matrix to the cellexalvrR object.
#' 
#' If the sample annotation data is not in 0/1 format please convert it using
#' make.cell.meta.from.df().
#'
#' @name addCellMeta2cellexalvr
#' @docType methods
#' @description Adds per cell metadata to a 'cellexalvrObj'
#' @param cellexalObj, cellexalvr object
#' @param cell.meta A matrix of cell metadata
#' @title Adds per cell metadata to a 'cellexalvrObj'
#' @export 
#if ( ! isGeneric('addCellMeta2cellexalvr') ){
setGeneric('addCellMeta2cellexalvr', ## Name
	function (cellexalObj, cell.meta) { 
		standardGeneric('addCellMeta2cellexalvr')
	}
)
#}


#' @rdname addCellMeta2cellexalvr
setMethod('addCellMeta2cellexalvr', signature = c ('cellexalvrR'),
	definition = function (cellexalObj, cell.meta) {

    rownames(cell.meta) <- colnames(cellexalObj@data)
    cellexalObj@meta.cell <- as.matrix(cell.meta)
    cellexalObj
} )


#' This function adds RNA velocity coordinates to an already existing set of dimension reduction coordinates. 
#' @name addVelocityToExistingDR
#' @docType methods
#' @description Adds RNA volocity data to an existing DR methods in an 'cellexalvrObj' object.
#' @param cellexalObj, cellexalvr object
#' @param velo.arrows A 6 column matrix describing RNA velocity arrows
#' @param dr.name The name of the DR coordinates being added to
#' @title Adds RNA volocity data to an existing DR methods in an 'cellexalvrObj' object
#' @export 
#if ( ! isGeneric('addVelocityToExistingDR') ){
setGeneric('addVelocityToExistingDR', ## Name
	function (cellexalObj, velo.arrows,dr.name) { 
		standardGeneric('addVelocityToExistingDR')
	}
)
#}


#' @rdname addVelocityToExistingDR 
setMethod('addVelocityToExistingDR', signature = c ('cellexalvrR'),
	definition = function (cellexalObj,velo.arrows,dr.name) {


	if(is.na(match(dr.name,names(cellexalObj@drc)))==T) {
		stop("There is no DR graph in your current CellexalObj that matches the one being added to")
	}else{ colnames(velo.arrows) <- c("dim1","dim2","dim3","velo1","velo2","velo3")
    	cellexalObj@drc[[dr.name]] <- velo.arrows[colnames(cellexalObj@data),]
    	cellexalObj
	}
} )



#' This function adds a table object into the index slot of the cellexalvrR obejct.
#'
#' @name addFACS2cellexalvr
#' @docType methods
#' @description  Adds FACS index to a cellexalvrObj
#' @param cellexalObj, cellexalvr object
#' @param facs.data A matrix of surface marker intensities
#' @title Adds FACS index to a cellexalvrObj
#' @export 
#if ( ! isGeneric('addFACS2cellexalvr') ){
setGeneric('addFACS2cellexalvr', ## Name
	function (cellexalObj, facs.data) { 
		standardGeneric('addFACS2cellexalvr')
	}
)
#}


#' @rdname addFACS2cellexalvr
setMethod('addFACS2cellexalvr', signature = c ('cellexalvrR'),
	definition = function (cellexalObj, facs.data) {

    rownames(facs.data) <- colnames(cellexalObj@data)
    cellexalObj@index <- as.matrix(facs.data)
    cellexalObj
} )


#' addDRC2cellexalvr is a helper function that applies some tests 
#' of usabilty to a 3D DRC matrix object and adds it to the cellexalvrR object.
#' 
#' @name addDRC2cellexalvr
#' @docType methods
#' @description  Adds drc coordinates to a 'cellexalvrObj'
#' @param cellexalObj, cellexalvr object
#' @param drcmatrix A (3 columns) matrix of coordinates
#' @param name A name for the object (default = graph<n>)
#' @title Adds drc coordinates to a 'cellexalvrObj'
#' @export 
#if ( ! isGeneric('renew') ){
setGeneric('addDRC2cellexalvr', ## Name
    function (cellexalObj, drcmatrix, name=NULL) { 
        standardGeneric('addDRC2cellexalvr')
    }
)
#}



#' @rdname addDRC2cellexalvr
setMethod('addDRC2cellexalvr', signature = c ('cellexalvrR'),
    definition = function (cellexalObj, drcmatrix, name=NULL) {

    rq.ind <- (length(cellexalObj@drc)+1)
    if ( ! is.null(name) ){
        rq.nm <- name
    }else {
        rq.nm <- paste("graph",(length(cellexalObj@drc)+1),sep="")
    }

    if ( ncol(drcmatrix ) == 3) {
        colnames( drcmatrix ) = c("dim1","dim2","dim3")
    }
    else if (ncol(drcmatrix ) == 6) {
        colnames( drcmatrix ) = c("dim1","dim2","dim3", 'velo1', 'velo2', 'velo3')
    }
    if ( is.null(rownames(drcmatrix)) ) {
        if (nrow(drcmatrix) == ncol(cellexalObj@data)) {
            rownames(drcmatrix) == colnames(cellexalObj@data)
        }else {
            stop(paste("The drc object", name, "needs rownames to be usable") )
        }
    }

    m = match( rownames(drcmatrix), colnames(cellexalObj@data) ) 
    if ( length(which(is.na(m))) > 0 ){
        ## Ooops - possibly a Ingest problem - that one adds -ref and -new to the ids!
        m = match( stringr::str_replace_all( rownames(drcmatrix), '-.*$' ,''),  colnames(cellexalObj@data) )
        if ( length(which(is.na(m))) > 0 ){
            stop( paste( "there were",length(which(is.na(m))),"new cells in the drc" ))
            }else {
                rownames(drcmatrix) = stringr::str_replace_all( rownames(drcmatrix), '-.*$' ,'')
            }
    }

    cellexalObj@drc[[rq.ind]] <- drcmatrix
    name = stringr::str_replace_all( name, '[/\\\\]+', '_' )
    names(cellexalObj@drc)[rq.ind] = name
    cellexalObj
} )


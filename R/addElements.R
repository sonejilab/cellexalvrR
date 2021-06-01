
#' This function adds a 0/1 information matrix to the cellexalvrR object.
#' 
#' If the sample annotation data is not in 0/1 format please convert it using
#' make.cell.meta.from.df(). #function definition in file 'make.cell.meta.from.df.R'
#'
#' @name addCellMeta2cellexalvr
#' @aliases addCellMeta2cellexalvr,cellexalvrR-method
#' @rdname addCellMeta2cellexalvr-methods
#' @docType methods
#' @description  Adds per cell metadata to a 'cellexalvrObj'
#' @param cellexalObj, cellexalvr object
#' @param cell.meta A matrix of cell metadata
#' @title description of function 'addCellMeta2cellexalvr'
#' @export addCellMeta2cellexalvr
#if ( ! isGeneric('renew') ){
setGeneric('addCellMeta2cellexalvr', ## Name
	function (cellexalObj, cell.meta) { 
		standardGeneric('addCellMeta2cellexalvr')
	}
)
#}

setMethod('addCellMeta2cellexalvr', signature = c ('cellexalvrR'),
	definition = function (cellexalObj, cell.meta) {

    rownames(cell.meta) <- colnames(cellexalObj@data)
    cellexalObj@meta.cell <- as.matrix(cell.meta)
    cellexalObj
} )


#' This function adds RNA velocity coordinates to an already existing set of dimension reduction coordinates. 
#' Should be a 6 column matrix of values describing the velocity arrows.
#'
#'
#' @name addVelocityToExistingDR
#' @aliases addVelocityToExistingDR,cellexalvrR-method
#' @rdname addVelocityToExistingDR-methods
#' @docType methods
#' @description  Adds RNA volocity data to an existing DR methods in an 'cellexalvrObj' object.
#' @param cellexalObj, cellexalvr object
#' @param velo.arrows A 6 column matrix describing RNA velocity arrows
#' @param dr.name The name of the DR coordinates being added to
#' @title description of function 'addVelocityToExistingDR'
#' @export addVelocityToExistingDR
#if ( ! isGeneric('renew') ){
setGeneric('addVelocityToExistingDR', ## Name
	function (cellexalObj, velo.arrows,dr.name) { 
		standardGeneric('addVelocityToExistingDR')
	}
)
#}

setMethod('addVelocityToExistingDR', signature = c ('cellexalvrR'),
	definition = function (cellexalObj,velo.arrows,dr.name) {


	if(is.na(match(dr.name,names(cellexalObj@drc)))==T) {
		stop("There is no DR graph in your current CellexalObj that matches the one being added to")
	}else{ colnames(velo.arrows) <- c("dim1","dim2","dim3","velo1","velo2","velo3")
    	cellexalObj@drc[[dr.name]] <- velo.arrows[colnames(cellexalObj@data),]
    	cellexalObj
	}
} )


#' addNewVelocity is a simple helper function that applies some tests 
#' of usabilty to a 3D DRC matrix object and adds it to the cellexalvrR object.
#' 
#' @name addNewVelocity
#' @aliases addNewVelocity,cellexalvrR-method
#' @rdname addNewVelocity-methods
#' @docType methods
#' @description  Adds drc coordinates to a 'cellexalvrObj'
#' @param cellexalObj, cellexalvr object
#' @param drcmatrix A (3 columns) matrix of coordinates
#' @param dr.name A name for the object (default = graph<n>)
#' @title description of function 'addNewVelocity'
#' @export addNewVelocity
#if ( ! isGeneric('renew') ){
setGeneric('addNewVelocity', ## Name
    function (cellexalObj, velo.arrows, dr.name) { 
        standardGeneric('addNewVelocity')
    }
)
#}

setMethod('addNewVelocity', signature = c ('cellexalvrR'),
    definition = function (cellexalObj, velo.arrows, dr.name) {

    if(is.na(match(dr.name,names(cellexalObj@drc)))==F) {
		stop("There is already a DR graph in your current CellexalObj that matches the name of the one being added as new.")
    }else{rq.ind <- (length(cellexalObj@drc)+1)
    
    colnames(velo.arrows) <- c("dim1","dim2","dim3","velo1","velo2","velo3")

    cellexalObj@drc[[dr.name]] <- velo.arrows[colnames(cellexalObj@data),]
    cellexalObj
    }
} )



#' This function simply adds a table object into the index slot of the cellexalvrR obejct.
#'
#' @name addFACS2cellexalvr
#' @aliases addFACS2cellexalvr,cellexalvrR-method
#' @rdname addFACS2cellexalvr-methods
#' @docType methods
#' @description  Adds FACS index to a cellexalvrObj
#' @param cellexalObj, cellexalvr object
#' @param facs.data A matrix of surface marker intensities
#' @title description of function 'addFACS2cellexalvr'
#' @export addFACS2cellexalvr
#if ( ! isGeneric('renew') ){
setGeneric('addFACS2cellexalvr', ## Name
	function (cellexalObj, facs.data) { 
		standardGeneric('addFACS2cellexalvr')
	}
)
#}

setMethod('addFACS2cellexalvr', signature = c ('cellexalvrR'),
	definition = function (cellexalObj, facs.data) {

    rownames(facs.data) <- colnames(cellexalObj@data)
    cellexalObj@index <- as.matrix(facs.data)
    cellexalObj
} )


#' addDRC2cellexalvr is a simple helper function that applies some tests 
#' of usabilty to a 3D DRC matrix object and adds it to the cellexalvrR object.
#' 
#' @name addDRC2cellexalvr
#' @aliases addDRC2cellexalvr,cellexalvrR-method
#' @rdname addDRC2cellexalvr-methods
#' @docType methods
#' @description  Adds drc coordinates to a 'cellexalvrObj'
#' @param cellexalObj, cellexalvr object
#' @param drcmatrix A (3 columns) matrix of coordinates
#' @param name A name for the object (default = graph<n>)
#' @title description of function 'addDRC2cellexalvr'
#' @export addDRC2cellexalvr
#if ( ! isGeneric('renew') ){
setGeneric('addDRC2cellexalvr', ## Name
    function (cellexalObj, drcmatrix, name=NULL) { 
        standardGeneric('addDRC2cellexalvr')
    }
)
#}

setMethod('addDRC2cellexalvr', signature = c ('cellexalvrR'),
    definition = function (cellexalObj, drcmatrix, name=NULL) {

    rq.ind <- (length(cellexalObj@drc)+1)
    if ( ! is.null(name) ){
        rq.nm <- name
    }else {
        rq.nm <- paste("graph",(length(cellexalObj@drc)+1),sep="")
    }
    mp <- drcmatrix
    colnames(mp) <- c("dim1","dim2","dim3")
    rownames(mp) <- colnames(cellexalObj@data)

    cellexalObj@drc[[rq.ind]] <- mp
    names(cellexalObj@drc)[rq.ind] <- rq.nm
    cellexalObj
} )


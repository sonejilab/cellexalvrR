#' MakeCellexalVRObj creates a CellexalVR object from a matrix object.
#' @name MakeCellexalVRObj 
#' @aliases MakeCellexalVRObj,matrix-method
#' @rdname MakeCellexalVRObj-methods
#' @docType methods
#' @description  Creates a cellexalvr objext with the data given
#' @param exdata A matrix of expression values (required). Colnames are cell IDs, rownames are unique gene names
#' @param drc.list A list of 3-column DRC coords (at least one required)
#' @param specie Specify whether data is from mouse or human (required)
#' @param cell.metadata Required meta data for cells
#' @param facs.data Surface marker intensities from index sorted cells
#' @keywords create cellexalvrR object
#' @title Create a cellexalvrR object from a matrix object.
#' @export MakeCellexalVRObj
if ( ! isGeneric('MakeCellexalVRObj') ){setGeneric('MakeCellexalVRObj', ## Name
	function (exdata,drc.list,specie=c("mouse","human"),cell.metadata=NULL,facs.data=NULL) { 
		standardGeneric('MakeCellexalVRObj') 
	}
) }


setMethod('MakeCellexalVRObj', signature = c ('dgCMatrix'),
	definition = function (exdata,drc.list,specie=c("mouse","human"),cell.metadata=NULL,facs.data=NULL) {

    if ( ! class(exdata) == 'dgCMatrix' ) {
		exdata = Matrix::Matrix(exdata, sparse=T)
	}


    ### add the rownames to the given matricies
    for(i in 1:length(drc.list)){
        rownames(drc.list[[i]]) <- colnames(exdata)

        if(ncol(drc.list[[i]])==3){
            colnames(drc.list[[i]]) <- c("dim1","dim2","dim3")
        }

        if(ncol(drc.list[[i]])==6){
            colnames(drc.list[[i]]) <- c("dim1","dim2","dim3","velo1","velo2","velo3")
        }
    }
    
    if(!is.null(cell.metadata)){
        rownames(cell.metadata) <- colnames(exdata)
    }

	cellexalobj <- methods::new("cellexalvrR",data=exdata,drc=drc.list)

    if(!is.null(cell.metadata)){
        cellexalobj  <- addCellMeta2cellexalvr(cellexalobj,cell.metadata) #function definition in file 'addElements.R'
    }

    if(!is.null(facs.data)){
        cellexalobj  <- addFACS2cellexalvr(cellexalobj,facs.data) #function definition in file 'addElements.R'
    }

    cellexalobj <- set.specie(cellexalobj,specie) #function definition in file 'cellexalvrFunctions.R'

    cellexalobj
} )

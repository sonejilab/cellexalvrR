#' The cell meta for cellexalvrR is a list of columns with 0/1 information for all groupings.
#' As this is no common format this function will convert a multi named grouping 
#' like CellType=c('HSC', MPP1', MPP2','HSC',...) into 3 columns that CellexalVR can use.
#' @name make.cell.meta.from.df
#' @aliases make.cell.meta.from.df,data.frame-method
#' @rdname make.cell.meta.from.df-methods
#' @docType methods
#' @description  Creates a meta cell matrix from a supplied dataframe from required fields
#' @param metad A dataframe of per cell metadata
#' @param rq.fields A vector of name specifiying which columns should me made into metadata
#' @keywords metadata cell
#' @title description of function make.cell.meta.from.df
#' @export make.cell.meta.from.df
if ( ! isGeneric('make.cell.meta.from.df') ){setGeneric('make.cell.meta.from.df', ## Name
	function (metad,rq.fields) { 
		standardGeneric('make.cell.meta.from.df') 
	}
) }

setMethod('make.cell.meta.from.df', signature = c ('data.frame'),
	definition = function (metad,rq.fields) {

    meta4cellexalvr <- NULL
    m = match(rq.fields, colnames(metad) )
    if ( length(which(is.na(m))) > 0 ) {
        err = c(paste( "The column names", paste( collapse=", ", rq.fields[which(is.na(m))]), "are not defined in the data.frame"),
            paste("data.frame's colnames are: ", paste( collapse=", ", colnames(metad))))
        stop( paste( err, collapse="\n") )
    }

    for(i in 1:length(rq.fields)){
        tmp.met <- phytools::to.matrix(metad[,rq.fields[i]], unique(metad[,rq.fields[i]]) )
        colnames(tmp.met) <- paste(rq.fields[i], colnames(tmp.met), sep="@")
        meta4cellexalvr <- cbind(meta4cellexalvr, tmp.met)
    }
	# match any repeat of whitespace and replace it with one '.' each
	colnames(meta4cellexalvr) <- gsub( '\\s+', '.', perl=T, colnames(meta4cellexalvr))
    rownames(meta4cellexalvr) = rownames(metad)
    meta4cellexalvr
} )


#' The cell meta for cellexalvrR is a list of columns with 0/1 information for all groupings.
#' As this is not common format this function will convert a multi named grouping 
#' like CellType=c('HSC', MPP1', MPP2','HSC',...) into 3 columns that CellexalVR can use.
#' @name makeCellMetaFromDataframe
#' @aliases makeCellMetaFromDataframe,data.frame-method
#' @rdname makeCellMetaFromDataframe-methods
#' @docType methods
#' @description  Creates a meta cell matrix from a supplied dataframe from required fields
#' @param metad A dataframe of per cell metadata
#' @param rq.fields A vector of name specifiying which columns should me made into metadata
#' @keywords metadata cell
#' @title description of function makeCellMetaFromDataframe
#' @export makeCellMetaFromDataframe
if ( ! isGeneric('makeCellMetaFromDataframe') ){setGeneric('makeCellMetaFromDataframe', ## Name
	function (metad,rq.fields) { 
		standardGeneric('makeCellMetaFromDataframe') 
	}
) }

setMethod('makeCellMetaFromDataframe', signature = c ('data.frame'),
	definition = function (metad,rq.fields) {

    meta4cellexalvr <- NULL

    for(i in 1:length(rq.fields)){
        tmp.met <- phytools::to.matrix(metad[,rq.fields[i]], unique(metad[,rq.fields[i]]) )
        colnames(tmp.met) <- paste(rq.fields[i], colnames(tmp.met), sep="@")
        meta4cellexalvr <- cbind(meta4cellexalvr, tmp.met)
    }
	# match any repeat of whitespace and replace it with one '.' each
	colnames(meta4cellexalvr) <- gsub( '\\s+', '.', perl=T, colnames(meta4cellexalvr))
    rownames(meta4cellexalvr) = rownames(metad)
    meta4cellexalvr
} )



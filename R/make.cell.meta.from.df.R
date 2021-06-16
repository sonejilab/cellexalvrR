#' The cell meta for cellexalvrR is a list of columns with 0/1 information for all groupings.
#' As this is no common format this function will convert a multi named grouping 
#' like CellType=c('HSC', MPP1', MPP2','HSC',...) into 3 columns that CellexalVR can use.
#' @name make.cell.meta.from.df
#' @docType methods
#' @description  Creates a meta cell matrix from a supplied dataframe from required fields
#' @param metad A dataframe of per cell metadata
#' @param rq.fields A vector of name specifiying which columns should me made into metadata
#' @keywords metadata cell
#' @title create a 0/1 table for a data.frame and a list of colnames
#' @export 
#if ( ! isGeneric('make.cell.meta.from.df') ){
setGeneric('make.cell.meta.from.df', ## Name
	function (metad,rq.fields) { 
		standardGeneric('make.cell.meta.from.df') 
	}
)
#}


#' @rdname make.cell.meta.from.df
setMethod('make.cell.meta.from.df', signature = c ('data.frame'),
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


#' The cell meta for cellexalvrR is a list of columns with 0/1 information for all groupings.
#' As this is not common format this function will convert a multi named grouping 
#' like CellType=c('HSC', MPP1', MPP2','HSC',...) into 3 columns that CellexalVR can use.
#' @name makeCellMetaFromDataframe
#' @docType methods
#' @description  Creates a meta cell matrix from a supplied dataframe from required fields
#' @param metad A dataframe of per cell metadata
#' @param rq.fields A vector of name specifiying which columns should me made into metadata
#' @keywords metadata cell
#' @title create a 0/1 table for a data.frame and a list of colnames
#' @export 
#if ( ! isGeneric('makeCellMetaFromDataframe') ){
setGeneric('makeCellMetaFromDataframe', ## Name
	function (metad,rq.fields) { 
		standardGeneric('makeCellMetaFromDataframe') 
	}
)
#}


#' @rdname makeCellMetaFromDataframe
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



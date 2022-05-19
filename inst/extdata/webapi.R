#' cellexalvrR WEB-API:
#' The WEB-API is trying to re-invent the connection beteen the CellexalVR fromt-end and the cellexalvrR back-end.
#' Instead of the previous file based server structure the R package plumber would create a http accessible R interface.
#' This will make the use of the function export2cellexalvr unnecessary.
library(R6)

Data <- R6Class("CellexalvrR_WebAPI",
  public = list(
    store = cellexalObj,
    initialize = function() {
    }
  )
)

Data = Data$new()


#* get expression values for one gene
#* @param name the gene name to fetch the data for
#* @post /GetCellValues
function( name ) {
	name = tolower(name)
	#return(name)
	id =  match( name, tolower(rownames(Data$store@data)))
	name = rownames(Data$store@data)[id]
	ret = Data$store@data[  name,]
	OK = which(ret != 0)
	if ( length(OK) == 0){
		return (paste("gene", name,"not found"))
	}
	## now we could even add this info to the log - or?
	drc = rev(names( Data$store@drc ))[1]
	if ( ! is.null(Data$store@usedObj$lastGroup)){
			drc = groupingInfo( Data$store, Data$store@usedObj$lastGroup)@drc
	}

	#Data$store = logExpression( Data$store, gene=name, drc = drc ) # does not work and has no use.
  Data$store = logExpression( Data$store, gene=name, drc = drc )
  Data$store =storeLogContents( Data$store, paste( format(Sys.time(), "%X" ), "Colored drcs by ", name ), type="Expression")
  id = length(Data$store@usedObj$sessionRmdFiles)
	Data$store = renderFile( Data$store, id, type='GeneExpr' )

	return( jsonlite::toJSON( list( ids = colnames(Data$store@data)[OK], values = as.character(ret[OK]) )))
}


#* get expression values for one gene - fast
#* @param name the gene name to fetch the data for
#* @post /GetExprs
function( name ) {
	name = tolower(name)
	#return(name)
	id =  match( name, tolower(rownames(Data$store@data)))
	name = rownames(Data$store@data)[id]
	ret = Data$store@data[  name,]
	OK = which(ret != 0)
	if ( length(OK) == 0){
		return (paste("gene", name,"not found"))
	}
	## now we could even add this info to the log - or?
	drc = rev(names( Data$store@drc ))[1]
	if ( ! is.null(Data$store@usedObj$lastGroup)){
			drc = groupingInfo( Data$store, Data$store@usedObj$lastGroup)@drc
	}

	return( jsonlite::toJSON( list( ids = colnames(Data$store@data)[OK], values = as.character(ret[OK]) )))
}

#* returns a list of genes that are differentially expressed
#* @param cellidfile:[file]
#* @param num.sig default 250
#* @param logfc.threshold default = 0.5
#* @param minPct default 0.2
#* @post /HeatmapList
function( cellidfile, num.sig = 250, logfc.threshold = 0.5, minPct = 0.2 ) {

	file = rawToChar(cellidfile[[1]], multiple = FALSE)
	ofile = filename(c("Selection", "txt" ), Data$store@outpath)
	write( file, ofile )
	return (paste ("not implemented but check", ofile) )

	Data$store = getDifferentials( Data$store, cellidfile=cellidfile, num.sig = num.sig,  logfc.threshold = logfc.threshold, minPct = minPct )
	return( jsonlite::toJSON( Data$store@usedObj$deg.genes ) )
}


#* returns a list of genes that are differentially expressed
#* @param cellidfile:[file]
#* @param name the gene name to correlate the data to
#* @param is.smarker gene or marker? defaulkt false 
#* @param cpp use the cpp pearson correlation (FASTER) default true
#* @post /cor2gene
function(name, ){

	output = filename(c("Correlation", name, "txt" ), Data$store@usedObj$sessionPath)
	Data$store = get.genes.cor.to(Data$store, gname=name, output=output, is.smarker=F, cpp=T)

}



#* Shut the server down
#* @get /shutdown
function(){
	if ( class( Data$store) == "cellexalvrR" ){
		Data$store = renderReport( Data$store )
	}
	q(save = "no") 
	return ("failed!")
}


#* Return "hello world"
#* @get /hello
function(){
  paste( "hello world\n", toString(Data$store) )
}

#* Return "hello world"
#* @param val
#* @post /change
function(val){
	Data$store = val
	return ("done")
}

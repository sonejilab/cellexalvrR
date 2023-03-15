#' merge merges two data sets
#' 
#' Will convert the names using homologene is the species are not compatible and will
#' return the names compatible with this@specie. Only the inner join will be returned!
#' 
#' @name merge
#' @docType methods
#' @description create two 2D drc plots for the report
#' @param this the cellexal object
#' @param other the other cellexalvrR object
#' @param addA a name added to this cell names (default '')
#' @param addB a name added to other cell names (default 'other')
#' @title create two 2D drc plots for the report
#' @export 
setGeneric('merge', ## Name
	function ( this, other,addA= "", addB="other" ) { 
		standardGeneric('merge')
	}
)





#' @rdname merge
setMethod('merge', signature = c ('cellexalvrR', 'cellexalvrR'),
	definition = function ( this, other, addA= "", addB="other" ){
		rename_cols = function(x, add) {
			translate = data.frame( orig = colnames(x@data), 
				new = paste( colnames(x@data), add, sep="_") )
			colnames(x@data) = translate[,2]
			rownames(x@meta.cell) = colnames(x@data)
			for (d in names(x@drc)) {
			    ok = match( rownames(x@drc[[d]]), translate[,1]  )
			    rownames(x@drc[[d]]) = translate[ok[which(!is.na(ok))],2]
			}
			return (x)
		}

		if (this@specie != other@specie ){
			if (!require("homologene", quietly = TRUE ) == T ) {
				stop("package 'homologene' needed for this function to work. Please install it.",
					call. = FALSE)
			}

			human_genes = function(x){
			        mapping_genes =  c(translating_tab[,1]) # mouse
			        OK = match( rownames(x@data), mapping_genes )
			        if (length(which(!is.na(OK))) > 10 ){ ## OK that were mouse genes
			                return( translating_tab[ OK[which(!is.na(OK))], 2] )
			        }
			        mapping_genes = c(translating_tab[,2]) # human
			        OK =  match( rownames(x@data), mapping_genes )
			        return( translating_tab[ OK[which(!is.na(OK))], 2] )
			}

			mouse_genes = function(x){
			        mapping_genes =  c(translating_tab[,1]) # mouse
			        OK = match( rownames(x@data), mapping_genes )
			        if (length(which(!is.na(OK))) > 10 ){ ## OK that were mouse genes
			                return( translating_tab[ OK[which(!is.na(OK))], 1] )
			        }
			        mapping_genes = c(translating_tab[,2]) # human
			        OK =  match( rownames(x@data), mapping_genes )
			        return( translating_tab[ OK[which(!is.na(OK))], 1] )
			}

			if ( this@specie == 'mouse' ){
				translating_tab = mouse2human( rownames(this@data) )
				translating_tab = translating_tab[ which(!is.na(match( translating_tab[,'humanGene'], rownames(other@data)))),]
				tab = table( unlist( lapply( c(this, other), mouse_genes ) ) )
				genes = names( tab[which(tab==2)] )
				translating_tab = translating_tab[ match(genes,translating_tab[,'mouseGene']), ]
				this = reduceTo( this, translating_tab[,'mouseGene'], what='row' )
				other= reduceTo( other,translating_tab[,'humanGene'], what='row' )
				rownames(other@data) = rownames(this@data)
			}
			else {
				translating_tab = mouse2human( rownames(other@data) )
				translating_tab = translating_tab[ which(!is.na(match( translating_tab[,'mouseGene'], rownames(other@data)))),]
				tab = table( unlist( lapply( c(this, other), human_genes ) ) )
				genes = names( tab[which(tab==2)] )
				translating_tab = translating_tab[ match(genes,translating_tab[,'humanGene']), ]
				this = reduceTo( this, translating_tab[,'humanGene'], what='row' )
				## can not be sure all mouse genes would be in the human data eitehr...
				other= reduceTo( other,translating_tab[,'mouseGene'], what='row' )
				rownames(other@data) = rownames(this@data)
			}
		}
		## gene names are now adjusted to fit the specie of "this"
		tab = table( c(rownames(this@data), rownames(other@data) ))
		genes = names(tab[which(tab == 2)])
		this = reduceTo( this, genes, what='row' )
		other= reduceTo( other,genes, what='row' )
		## renaming the col names
		this = rename_cols( this, addA )
		other = rename_cols( other, addB )

		# construct the return value
		ret = new('cellexalvrR')
		ret@specie = this@specie
		ret@data = Matrix.utils::merge.Matrix(this@data, other@data, 
			rownames(this@data), rownames(other@data), fill.x=TRUE, fill.y=TRUE )
		ret@data = ret@data[rownames(this@data),]
		ret@meta.cell = as.matrix( data.frame("index" = colnames(ret@data), save=rep ("s", ncol(ret@data)) ) )
		browser()
		if ( addA == "" ){
			ret@drc = this@drc
		}else {
			ret = GrabDRCs( ret, this, addA )
		}
		ret = GrabDRCs( ret, other, addB )

		if ( addA == ""){
			addA = "this"
		}

		## and then the sample table - that will be horrible!
		if ( is.null( this@usedObj$samples ) ){
			
			this@usedObj$samples = data.frame( "index" = colnames(this@data), sname= rep(addA, ncol(this@data)))
		}
		if (  is.null( this@usedObj$samples$sname )){
			this@usedObj$samples$sname = rep(addA, ncol(this@data))
		}
		if ( is.null( other@usedObj$samples ) ){
			if ( addB == ""){
				addB = "other"
			}
			other@usedObj$samples = data.frame( "index" = colnames(other@data), sname= rep(addB, ncol(other@data)))
		}
		if (  is.null( other@usedObj$samples$sname )){
			other@usedObj$samples$sname = rep(addB, ncol(other@data))
		}
		tab = table ( c( colnames(this@usedObj$samples), colnames(other@usedObj$samples) ) )
		common = names(tab[which(tab == 2)])
		ret@usedObj$samples = rbind(this@usedObj$samples[,common], other@usedObj$samples[,common] )
		warning( paste( sep="\n",
			"please make sure the meta.cell data contains what you expect and adjust if necessary." ,
			"Easiest should be from the @usedObj$samples data.frame in the returned celleclvrR object.",
			"retValue@meta.cell = make.cell.meta.from.df( retValue@usedObj$samples, c('your columns') )" ) )
		ret@meta.cell = make.cell.meta.from.df( ret@usedObj$samples, c('sname'))

		return ( ret )
	}
)

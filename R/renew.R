#' renew is trying to update objects from old versions to the most up to date structure.
#' 
#' It is tested to work on version <= 0.14.5.
#' 
#' @name renew
#' @docType methods
#' @description  updatae the class definition by re-creating the instance This version also makes sure,
#' @description  that the returned object is an S4 object.
#' @param x the object you want to updatae
#' @title renew/update a cellexalvrR object structure to the last version
#' @export renew
#if ( ! isGeneric('renew') ){
setGeneric('renew', ## Name
	function ( x ) {
		standardGeneric('renew')
	}
)
#}



#' @rdname renew
setMethod('renew', signature = c ('cellexalvrR'),
	definition = function ( x ) {
			#ret <- new("cellexalvrR",data=as.matrix(x@data),drc=x@drc,meta.cell=x@meta.cell,meta.gene = x@meta.gene,  index = x@index, tfs= x@tfs)
			ret = NULL
			#browser()
			if( isS4(x)) {
				## OK no R6 then ;-)
				ret = x
				if ( ! .hasSlot( x, 'version') ) {
					if ( .hasSlot( x, 'dat') & .hasSlot(x, 'mds') ) {
						ret <- new("cellexalvrR",data=x@dat,drc=x@mds,meta.cell=x@meta.cell, specie=x@specie,
							meta.gene = x@meta.gene,  index = x@index, outpath= x@outpath)
					}else if ( .hasSlot( x, 'data') & .hasSlot(x, 'drc')) {
						ret <- new("cellexalvrR",data=Matrix(x@data, sparse=T),drc=x@drc, specie=x@specie,
							meta.cell=x@meta.cell,meta.gene = x@meta.gene,  index = x@index, outpath= x@outpath)
					}else if ( .hasSlot( x, 'mds')) {	
						ret <- new("cellexalvrR",data=Matrix(x@data, sparse=T),drc=x@mds, specie=x@specie,
							meta.cell=x@meta.cell,meta.gene = x@meta.gene,  index = x@index, outpath= x@outpath)
					}else {
						print ( "Sorry this need re-coding - how do we update this old object here?")
						if(interactive()) { browser() }
					}
					
				}else if (x@version != as.character(packageVersion("cellexalvrR"))  ) { 
					ret <- new("cellexalvrR",data=Matrix(x@data, sparse=T),drc=x@drc, specie=x@specie,
						meta.cell=x@meta.cell,meta.gene = x@meta.gene,  index = x@index, outpath= x@outpath)
				}
				#browser()
				if( methods::.hasSlot(x,'userGroups') ){
					ret@userGroups = x@userGroups
				}
				if( methods::.hasSlot(x,'colors') ){
					ret@colors = x@colors
				}
				if( methods::.hasSlot(x,'usedObj') ){
					ret@usedObj = x@usedObj
				}
				if( methods::.hasSlot(x,'specie') ){
					ret@specie = x@specie
				}
				ret@groupSelectedFrom = x@groupSelectedFrom
				ret@index = x@index

			}else {
				if ( is.null(x$index)){
					x$index = matrix()
				}

				ret <- methods::new("cellexalvrR",data=Matrix(x$data, sparse=T),drc=x$mds,
					meta.cell=as.matrix(x$meta.cell),meta.gene = as.matrix(x$meta.gene),  index = x$index, specie= 'unset!',
					 outpath= x@outpath)
				if( methods::.hasSlot(x,'userGroups') ){
					ret$userGroups = x$userGroups
				}
				if( methods::.hasSlot(x,'colors') ){
					ret$colors = x$colors
				}
				if( methods::.hasSlot(x,'usedObj') ){

					ret$usedObj = x$usedObj
				}
				ret$groupSelectedFrom = x$groupSelectedFrom
				ret$index = x$index
			}
			## now lets add the inbuilt groupings...
#			for ( name in c('TFs', 'epigenetic', 'CellCycle', 'CellSurface') ) {
#				if ( is.na( match(name, colnames(ret@meta.gene))) ) {
#					useInbuiltGOIlists ( ret, name ) #function definition in file 'useInbuiltGOIlists.R'
#				}
#			}

			invisible(ret)
}  )


#' @rdname renew
setMethod('renew', signature = c ('cellexalvr'), ## old R3 object
		definition = function (x) {
			class(x) = 'cellexalvrR'
			renew(x) #function definition in file 'renew.R'
		} )

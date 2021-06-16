#' @name useInbuiltGOIlists
#' @docType methods
#' @description  An easy function to register the inbuilt (G)enes (O)f (I)nterest lists.
#' @param x, cellexalvr object
#' @param name the name of the inbuilt list to use ( either 'TFs' or 'epigenetic' for now)
#' @param x, TEXT MISSING
#' @param name one of ("TFs", 'epigenetic', "CellCycle", "CellSurface")
#' @title easily identify e.g. all TFs in the dataset
#' @export 
#if ( ! isGeneric('useInbuiltGOIlists') ){
setGeneric('useInbuiltGOIlists', ## Name
	function (x, name ) { 
		standardGeneric('useInbuiltGOIlists') 
	}
)
#}


#' @rdname useInbuiltGOIlists
setMethod('useInbuiltGOIlists', signature = c ('cellexalvrR'),
	definition = function (x, name ) {
			
		if ( length(colnames(x@meta.gene)) == 0){
			x@meta.gene = matrix( ncol=1, rownames(x@data) )
			colnames(x@meta.gene) = "gene_id"
		}
			if ( ! is.na( match(name, colnames(x@meta.gene)))) {
				message( "This GIO list has already been defined" )
				return ( invisible(x) )				
			}
			
			if ( name == "TFs" ) {
				## register TFs
				#if ( ! is.na(x@tfs) ){
				#	x = defineGOIs( x, 'TFs', x@tfs[!x@tfs==""] ) #function definition in file 'defineGOIs.R'
				#}
				#else {
					## use inbuilt lists
					hum_t <- length(which(is.na(match(rownames(x@data), cellexalvrR::human.tfs))==F))
					mouse_t <- length(which(is.na(match( rownames(x@data), cellexalvrR::mouse.tfs))==F))
					if (hum_t > mouse_t ){
						x = defineGOIs( x, name, cellexalvrR::human.tfs ) #function definition in file 'defineGOIs.R'
						x@specie = 'human'
					}else if ( mouse_t > hum_t ){
						x = defineGOIs( x, name, cellexalvrR::mouse.tfs ) #function definition in file 'defineGOIs.R'
						x@specie = 'mouse'
					}else {
						stop( "Sorry, but neither inbuilt dataset (Gene Symbols from mouse and humans) do match to the rownames(data$data) - please double ckech that.")
					}
				#}
			}
			else if ( name == 'epigenetic' ) {
				# register 'epigeneic'
				hum_e <- length(which(is.na(match(rownames(x@data), cellexalvrR::Epigenetic$HGNC_symbol))==F))
				mouse_e <- length(which(is.na(match( rownames(x@data), cellexalvrR::Epigenetic$MGI_symbol ))==F))
				if ( hum_e > mouse_e){
					x = defineGOIs( x, name, cellexalvrR::Epigenetic$HGNC_symbol, cellexalvrR::Epigenetic$Target ) #function definition in file 'defineGOIs.R'
					x@specie = 'human'
				}else if ( mouse_e > hum_e ){
					x = defineGOIs( x, name, cellexalvrR::Epigenetic$MGI_symbol, cellexalvrR::Epigenetic$Target) #function definition in file 'defineGOIs.R'
					x@specie = 'mouse'
				}else {
					stop( "Sorry, but neither inbuilt dataset (Gene Symbols from mouse and humans) do match to the rownames(data$data) - please double ckech that.")
				}
			}
			else if ( name =="CellCycle" ) {
				hum_e <- length(which(is.na(match(rownames(x@data), cellexalvrR::CellCycle$Gene.Symbol))==F))
				mouse_e <- length(which(is.na(match( rownames(x@data) ,cellexalvrR::CellCycle$MouseGene ))==F))
				if ( hum_e > mouse_e){
					x = defineGOIs( x, name, cellexalvrR::CellCycle$Gene.Symbol, cellexalvrR::CellCycle$X ) #function definition in file 'defineGOIs.R'
				}else if ( mouse_e > hum_e ){
					x = defineGOIs( x, name, cellexalvrR::CellCycle$MouseGene, cellexalvrR::CellCycle$X ) #function definition in file 'defineGOIs.R'
				}else {
					stop( "Sorry, but neither inbuilt dataset (Gene Symbols from mouse and humans) do match to the rownames(data$data) - please double ckech that.")
				}
			}
			else if ( name =="CellSurface" ) {
				hum_e <- length(which(is.na(match(rownames(x@data), cellexalvrR::human.CellSurface))==F))
				mouse_e <- length(which(is.na(match( rownames(x@data), cellexalvrR::mouse.CellSurface ))==F))
				if ( hum_e > mouse_e){
					x = defineGOIs( x, name, cellexalvrR::human.CellSurface) #function definition in file 'defineGOIs.R'
				}else if ( mouse_e > hum_e ){
					x = defineGOIs( x, name, cellexalvrR::mouse.CellSurface ) #function definition in file 'defineGOIs.R'
				}else {
					stop( "Sorry, but neither inbuilt dataset (Gene Symbols from mouse and humans) do match to the rownames(data$data) - please double ckech that.")
				}
			}
			else {
				stop ( paste("Sorry, but the gene list", name, "is not defined" ) )
			}
			x
		}  )

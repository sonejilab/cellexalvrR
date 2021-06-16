
#' This is a function called from CellexalVR.
#' 
#' Its main function is to store the result from getDifferentials() in a file  #function definition in file 'getDifferentials.R'
#' to be read by CellexalVR.
#' @name make.cellexalvr.heatmap.list
#' @docType methods
#' @description  Creates an outfile for CellexalVR containing a list of genes from which a heatmap is made in VR.
#' Internally this function uses the getDifferentials() function and writes gene list to the outfile.
#' @param cvrObj A cellexalvr object
#' @param cellidfile file containing cell IDs or the grouping name
#' @param num.sig The number of differentials to be returned
#' @param outfile The name of the output file
#' @param stats_method the stats method to use see getDifferentials()
#' @title Create the gene list files for VR
#' @keywords differential, gene list
#' @export 
#if ( ! isGeneric('renew') ){
setGeneric('make.cellexalvr.heatmap.list', ## Name
			function (cvrObj,cellidfile,num.sig,outfile, stats_method='wilcox' ) { 
				standardGeneric('make.cellexalvr.heatmap.list') 
			}
)
#}



#' @rdname make.cellexalvr.heatmap.list
setMethod('make.cellexalvr.heatmap.list', signature = c ('cellexalvrR'),
		definition = function (cvrObj,cellidfile,num.sig,outfile, stats_method='wilcox') {


			if ( file.exists( paste(sep=".", outfile, 'sqlite3')) ){
				unlink( paste(sep=".", outfile, 'sqlite3') )
			}
			
			if ( ! file.exists( file.path(cvrObj@outpath, basename(cellidfile)) ) ){
				file.copy( cellidfile, cvrObj@outpath )
			}

			cvrObj = getDifferentials(cvrObj,cellidfile, stats_method, num.sig= num.sig ) #function definition in file 'getDifferentials.R'
			#getDifferentials(cvrObj,cellidfile, stats_method, num.sig= num.sig) #function definition in file 'getDifferentials.R'

			cvrObj@groupSelectedFrom[[cvrObj@usedObj$lastGroup]]@heatmapBasename = basename( outfile )

			if ( length( grep('Time', cvrObj@groupSelectedFrom[[cvrObj@usedObj$lastGroup]]@gname)) > 0 ){
				partentGroup = getTime( cvrObj, cvrObj@groupSelectedFrom[[cvrObj@usedObj$lastGroup]]@gname )@parentSelection
				cvrObj@groupSelectedFrom[[partentGroup]]@heatmapBasename = basename( outfile )
			}
			gene.cluster.order = cvrObj@usedObj$deg.genes

			message (paste( "trying to write file", outfile, "containing", length(gene.cluster.order), "genes") )
			write(c(length(gene.cluster.order),gene.cluster.order), file=outfile,ncolumns=1)
			## probably a good way to export the information as database, too.
			## we only need the GOIs
			tmp = reduceTo(cvrObj, what='row', to=  gene.cluster.order ) #function definition in file 'reduceTo.R'
			## we also only need the samples that have been selected:
			tmp = reduceTo(tmp, what='col', to=  colnames(cvrObj@data)[ #function definition in file 'reduceTo.R'
				which(!is.na(cvrObj@userGroups[,cvrObj@usedObj$lastGroup])) ] )
			if ( file.exists(paste(sep=".", outfile, 'sqlite3'))){
				unlink(paste(sep=".", outfile, 'sqlite3') )
			}
			try ( write_as_sqlite3( tmp, paste(sep=".", outfile, 'sqlite3') ) ) #function definition in file 'ExportFunctions.R'
			invisible( cvrObj )
		} )


#' @rdname make.cellexalvr.heatmap.list
setMethod('make.cellexalvr.heatmap.list', signature = c ('character'),
		definition = function (cvrObj,cellidfile,num.sig,outfile, stats_method=NA ) {
			cvrObj <- loadObject( cvrObj ) #function definition in file 'lockedSave.R'
			make.cellexalvr.heatmap.list(cvrObj,cellidfile,num.sig,outfile, stats_method ) #function definition in file 'make.cellexalvr.heatmap.list.R'
		}
)

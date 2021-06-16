#' This function creates all files necessary 
#' for the CellexalVR application to show this data.
#'
#' The folder with this data needs to resude in the CellexalVR\\DATA folder.
#' Consult the CellexalVR documentation for further path requrements.
#' @name export2cellexalvr
#' @docType methods
#' @description  Creates the base files needed to run the VR environment
#' @param x A cellexalvr object
#' @param path the oputpath to store the data in
#' @param forceDB re-write the db even if it exisis (default =F)
#' @title create the VR data folder necessary for CellexalVR
#' @examples \dontrun{
#' dir.create ('data')
#' export2cellexalvr(x, path='data') #function definition in file 'ExportFunctions.R'
#' }
#' @export export2cellexalvr
#if ( ! isGeneric('export2cellexalvr') ){
setGeneric('export2cellexalvr', ## Name
	function (x,path, forceDB=FALSE ) { 
		standardGeneric('export2cellexalvr') 
	}
)
#}


#' @rdname export2cellexalvr
setMethod('export2cellexalvr', signature = c ('cellexalvrR'),
	definition = function (x,path, forceDB=FALSE ) {

	## check that the cell names (all rownames) contain no spaces!!
	old_names = colnames(x@data)
	good_names = stringr::str_replace_all( colnames(x@data),'\\s+', '_')

	if ( forceDB ) {
		 if (file.exists(path)){
		 	unlink( path, recursive=TRUE)
		 	dir.create( path )
		 }
	}
	if ( ! file.exists(path)){
		warning("path does not exists - creating it now")
		dir.create( path )
	}
	for ( n in names(x@drc) ) {
		if ( is.null(rownames( x@drc[[n]]))){
			rownames( x@drc[[n]]) = good_names
		}
	}
	if ( nrow(x@index) == ncol(x@data) ){
		rownames( x@index ) = good_names
	}

	x = check(x)
	if (! x@usedObj$checkPassed ){
		stop( "The cellexalvrR object did not pass the internal checks")
	}

	ofile = file.path( path, "cellexalObj.RData")
	if ( ! file.exists( ofile) ){
		x@outpath = ''
		cellexalObj = x
		save(cellexalObj, file=ofile )
	}
    

    #utils::write.table(x@data,paste(path,"expression.expr",sep=""),row.names=T,col.names=NA,quote=F,sep="\t",eol="\n")
	ofile = file.path(path,"a.meta.cell")
	if ( ! file.exists( ofile) ){
		if ( nrow( x@meta.cell) == ncol( x@data ) ) {
			rownames(x@meta.cell) = good_names
			utils::write.table(x@meta.cell,ofile,row.names=T,col.names=NA,quote=F,sep="\t",eol="\n")
		}
	}
	ofile = file.path(path,"index.facs")
	if ( ! file.exists( ofile) ){
		if ( nrow(x@index) == ncol(x@data) ){
			utils::write.table(x@index,ofile,row.names=T,col.names=NA,quote=F,sep="\t",eol="\n")
		}
	}
	ofile = file.path(path,"c.meta.gene")
	if ( ! file.exists( ofile) ){
		utils::write.table(x@meta.gene,ofile,row.names=T,col.names=NA,quote=F,sep="\t",eol="\n")
	}

    for(i in 1:length(x@drc)){
        

		#if(ncol(x@drc[[i]])==3){

			ofile = file.path(path,paste(names(x@drc)[i],".mds",sep=""))
			if ( ! file.exists( ofile )) {
				#utils::write.table(x@drc[[i]],ofile,row.names=T,col.names=F,quote=F,sep="\t",eol="\n")
				utils::write.table(data.frame("CellID"=rownames(x@drc[[i]]),x@drc[[i]]),ofile,row.names=F,col.names=T,quote=F,sep="\t",eol="\n")
			}
		#}

	}
	
	ofile = file.path(path,"database.sqlite")
	if ( file.exists( ofile ) ) {
		if ( forceDB ){
			unlink( ofile ) ## always create the database?!
			message("old database removed")
		}
	}
	if ( ! file.exists(ofile) || forceDB==T ) {
	    #genes <- tolower(rownames(x@data))

	    #if ( file.exists( ofile ) ) {
	    #	## database should be re-reacted?!
	    #	unlink(ofile)
	    #}
	    oldw <- getOption("warn")
		options(warn = -1)

		genes <- rownames(x@data)
		t = table(tolower(genes))
		if ( length(which(t > 1)) > 0 ){
			bad = match( names(which(t > 1)), tolower(genes) )
			genes[bad] = paste(sep="_", genes[bad], 1)
		}
		genes <- data.frame( 'id' = 1:length(genes), genes= genes )

		cells <- data.frame( 'id'= 1:ncol(x@data), sample= colnames(x@data) )
		
		## melt the sparse matrix using the toColNums Rcpp function
		#mdc = FastWilcoxTest::ZScore( x@data )
		#colnames(mdc) <- colnames(x@data)
		#rownames(mdc) <- rownames(x@data)
		#mdc = FastWilcoxTest::meltSparseMatrix( mdc )
		colnames(genes) <- c('id', 'gname')
		colnames(cells) <- c('id','cname')
		
	
    	con <- RSQLite::dbConnect(RSQLite::SQLite(),dbname = ofile )
		
		RSQLite::dbSendStatement(con,
			"create table genes ('id' integer not null unique,'gname' varchar(20) COLLATE NOCASE)")
        
		RSQLite::dbSendStatement(con,
			"create table cells ('id' integer not null unique,'cname' varchar(20) COLLATE NOCASE)")

		RSQLite::dbWriteTable(con, "genes", genes, append =TRUE) ## append=TRUE as we create the table
		RSQLite::dbWriteTable(con, "cells", cells, append =TRUE)

		RSQLite::dbSendStatement(con, 
			"CREATE UNIQUE INDEX gnameIDX on genes ( gname )")
		RSQLite::dbSendStatement(con, 
			"CREATE UNIQUE INDEX cnameIDX on cells ( cname )")
		
		#mdc = FastWilcoxTest::meltSparseMatrix( x@data )
		#RSQLite::dbWriteTable(con, "datavalues",mdc, append = FALSE, overwrite=TRUE)
		tfile = file.path(path, 'tmp.data.txt')
		if ( file.exists(tfile)){
			unlink(tfile)
		}
		FastWilcoxTest::sparse2SQLite_text_file( x@data, file=tfile, sep="," )
		RSQLite::dbSendStatement(con, paste(
			"CREATE TABLE `datavalues` ( `gene_id` INTEGER,",
			" `cell_id` INTEGER, `value` REAL )" ))
		RSQLite::dbWriteTable(con, tfile, header=FALSE, name='datavalues', append=TRUE )

		unlink(tfile)

		RSQLite::dbSendStatement(con,
			"create index gene_id_data ON datavalues ( 'gene_id' )")
		RSQLite::dbSendStatement(con,
			"create index cell_id_data ON datavalues ( 'cell_id' )")

    	RSQLite::dbDisconnect(con)

    	options(warn = oldw)

	}
	invisible(x)

} )

#' SQLite is the database the VR reads the expression values from.
#' 
#' This function creates the databases from the data stored in the data slot.
#' 
#' @name write_as_sqlite3
#' @docType methods
#' @description save the x@data object without questions asked.
#' @param x the cellexalvrR object
#' @param ofile the database outfile
#' @title write a cellexalvrR object's data to a 'sqlite3' database of name ofile.
#' @export 
setGeneric('write_as_sqlite3', ## Name
		function ( x, ofile )  { ## Argumente der generischen Funktion
			standardGeneric('write_as_sqlite3') ## der Aufruf von standardGeneric sorgt für das Dispatching
		}
)

 

#' @rdname write_as_sqlite3
setMethod('write_as_sqlite3', signature = c ('cellexalvrR'),
		definition = function ( x, ofile )  {
			
			oldw <- getOption("warn")
			options(warn = -1)
			
			genes <- rownames(x@data)
			genes <- data.frame( 'id' = 1:length(genes), genes= genes )
			
			cells <- data.frame( 'id'= 1:ncol(x@data), sample= colnames(x@data) )
			
			## melt the sparse matrix using the toColNums Rcpp function
			mdc = FastWilcoxTest::meltSparseMatrix( x@data )
			
			colnames(genes) <- c('id', 'gname')
			colnames(cells) <- c('id','cname')
			
			con <- RSQLite::dbConnect(RSQLite::SQLite(),dbname = ofile )
			
			RSQLite::dbWriteTable(con, "datavalues",mdc)
			#RSQLite::dbClearResult(RSQLite::dbListResults(con)[[1]])
			RSQLite::dbSendStatement(con,"create table genes ('id' integer not null unique,'gname' varchar(20) COLLATE NOCASE)")
			
			RSQLite::dbSendStatement(con,"create table cells ('id' integer not null unique,'cname' varchar(20) COLLATE NOCASE)")
			
			RSQLite::dbWriteTable(con, "genes", genes, append = TRUE)
			RSQLite::dbWriteTable(con, "cells", cells, append = TRUE)

			#RSQLite::dbClearResult(RSQLite::dbListResults(con)[[1]])
			#print("OK1")

			RSQLite::dbSendStatement(con, "CREATE UNIQUE INDEX gnameIDX on genes ( gname )")
			#RSQLite::dbClearResult(RSQLite::dbListResults(con)[[1]])
			#print("OK2")
			RSQLite::dbSendStatement(con, "CREATE UNIQUE INDEX cnameIDX on cells ( cname )")
			#RSQLite::dbClearResult(RSQLite::dbListResults(con)[[1]])
			#print("OK3")

			RSQLite::dbSendStatement(con,"create index gene_id_data ON datavalues ( 'gene_id' )")
			#RSQLite::dbClearResult(RSQLite::dbListResults(con)[[1]])
			#print("OK4")
			RSQLite::dbSendStatement(con,"create index cell_id_data ON datavalues ( 'cell_id' )")
			RSQLite::dbClearResult(RSQLite::dbListResults(con)[[1]])
			#print("OK5")
			
			#RSQLite::dbClearResult(con)
			RSQLite::dbDisconnect(con)
			options(warn = oldw)
		} )


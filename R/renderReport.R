#' renderReport is the final step to create a html log file.
#' 
#' These log files can be accessed from within the VR environment using the inbuilt browser.
#' It also creates a zip file that contains all data required to view this report.
#' 
#' @name renderReport
#' @docType methods
#' @description after one session this function renders the output as gitbook
#' @param cellexalObj the cellexlvrR object
#' @title after one session this function renders the output as gitbook
#' @export 
setGeneric('renderReport', ## Name
	function (cellexalObj) { 
		standardGeneric('renderReport')
	}
)



#' @rdname renderReport
setMethod('renderReport', signature = c ('cellexalvrR'),
	definition = function (cellexalObj) {
	## here you should know which files to render  ;-)
	if ( is.null( cellexalObj@usedObj$sessionPath )){
		cellexalObj = sessionPath(cellexalObj) #function definition in file 'sessionPath.R'
	}
	sessionPath = normalizePath(cellexalObj@usedObj$sessionPath)

	endText = paste("## Session End" , stringr::str_replace_all(R.utils::timestamp(quiet=T), '[#-]', '' ), sep="\n\n")

	Rlog = file.path( cellexalObj@outpath, paste( sep=".",'mainServer', Sys.getpid(), 'output') )
	
	if ( file.exists( Rlog ) ){
			file.copy( Rlog, file.path( cellexalObj@usedObj$sessionPath, 'Rlog.txt') )
			endText = paste(sep="",endText, "The R log of this session can be downloaded from <a href='",
				file.path(cellexalObj@usedObj$sessionName, 'Rlog.txt'),"' download>here</a>.")
			unlink( Rlog )
	}

	cellexalObj = storeLogContents( cellexalObj, endText, type='End') 


	for ( i in 1:length(cellexalObj@usedObj$sessionRmdFiles) ){
		cellexalObj@usedObj$sessionRmdFiles[i] = normalizePath(cellexalObj@usedObj$sessionRmdFiles[i])
	}

	fileConn<-file(file.path(sessionPath,  '_bookdown.yml') )
	writeLines(c(
		paste('book_filename:', cellexalObj@usedObj$sessionName),
		'output_dir: ../',			
		'delete_merged_file: true' 
        ), fileConn )
    close(fileConn)
	
	oldwd = getwd()
	setwd( cellexalObj@usedObj$sessionPath )
	files = as.character(unlist(lapply( cellexalObj@usedObj$sessionRmdFiles, basename)))
	message( 'bookdown::render_book' )
	## and now a bloody hack:list.files( './', filter='*.Rmd')
	cmd = paste(sep="", "bookdown::render_book( input= list.files( './', pattern='*.Rmd'),",
		" output_format='bookdown::gitbook',
		 "
		,"config_file = '_bookdown.yml' )" )
	script= 'runRender.R'
	if ( file.exists( script)) {
		unlink( script )
	}
	cat( cmd, file=script , append=FALSE)
	Rscript = file.path( R.home(),"bin","Rscript" )
	if ( ! file.exists(Rscript)){
		Rscript = file.path( R.home(),"bin","Rscript.exe" )
	}
	Rscript = paste(sep="", '"', Rscript,'"')
	tryCatch( {
		system( paste(Rscript, script) , intern=TRUE)
	} , error=function(err) { 
		print ( paste(sep="\n","renderReport: Pandoc call failed:" , err) )
	} )

	system( paste(Rscript, script))
	#bookdown::render_book( input=files, , 
	setwd( oldwd )
	
	expected_outfile =  paste(sep="-","session-log-for-session",
		tolower(cellexalObj@usedObj$sessionName))
	expected_outfile = stringr::str_replace_all( expected_outfile, '_', '-')
	#expected_outfile = file.path( cellexalObj@outpath,expected_outfile )

	expected_outfiles <-  list.files(cellexalObj@outpath , 
			full.names = TRUE, pattern='*.html')
	OK = length( grep ( expected_outfile, expected_outfiles) > 0)

	if ( OK ){

		## get rid of all section html files
		expected_outfiles = expected_outfiles[grep ( expected_outfile, expected_outfiles)]
		htmls <-  list.files( cellexalObj@outpath, full.names = TRUE, pattern='*.html')
		mine = htmls[ grep(paste( sep="", '_',cellexalObj@usedObj$sessionName) , htmls )]
		do.call(file.remove, list(mine))

		zfile = paste(sep=".",paste(sep="_",'PortableLog',cellexalObj@usedObj$sessionName),'zip')
		
		files = c(
			sapply(expected_outfiles, basename), 
			cellexalObj@usedObj$sessionName, 
			'libs'
		)
		old= getwd()
		setwd( cellexalObj@outpath )
		if ( file.exists( zfile )){
			unlink(zfile)
		}
		zip::zip(zfile, files )
		setwd(old)
	
		## get rid of session information
		cellexalObj@usedObj$sessionPath = 
		 cellexalObj@usedObj$sessionRmdFiles =
		 cellexalObj@usedObj$sessionName = NULL
		#savePart(cellexalObj,part = 'usedObj' ) #function definition in file 'integrateParts.R'
		
		unlink( file.path( cellexalObj@outpath , 'mainServer.sessionName') )
		
	}else {
		print ( paste( "some error has occured - output ",expected_outfiles[1]," file was not created!" ))
	}	

	lockedSave( cellexalObj) #function definition in file 'lockedSave.R'
	
	cellexalObj
} )


#' @rdname renderReport
setMethod('renderReport', signature = c ('character'),
		definition = function (cellexalObj) {
			cellexalObj <- loadObject(cellexalObj) #function definition in file 'lockedSave.R'
			renderReport(cellexalObj ) #function definition in file 'renderReport.R'
		}
)



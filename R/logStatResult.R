#' logStatResult will create a clickable link in the log document 
#' where the user can download the stats table from.
#' @name logStatResult
#' @docType methods
#' @description Create a log file part including a stats data download link (coma separted table file)
#' @param x the cellexalvrR object
#' @param method the stats method used (no spaces please)
#' @param data the results table
#' @param col the p value column to plot the -log10 histogram
#' @param text an optional additional text shown after the heading
#' @param nameAdd add this to the group name for the ToppGene output file. This allows for e.g. a sub analysis of a main clustering and some concatenation of the ToppGene files.
#' @param showIDs plot the IDs on the drc plots. This will get problems if it is many IDs (default TRUE)
#' @title make the stats table available in the log system
#' @export
setGeneric('logStatResult', ## Name
	function ( x, method, data, col=NULL, text=NULL, nameAdd=NULL, showIDs = TRUE ) { ## Argumente der generischen Funktion
		standardGeneric('logStatResult') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)



#' @rdname logStatResult
setMethod('logStatResult', signature = c ('cellexalvrR'),
	definition = function ( x, method, data, col=NULL, text=NULL, nameAdd=NULL, showIDs = TRUE ) {
	if (! is.null( x@usedObj$sessionName)) {

		## export the data into a file and add a small download link into the report
		opath = file.path( x@usedObj$sessionPath,'tables')
		if ( ! file.exists( opath )) {
			dir.create( opath )
		}

		## get a unique file not overwriting old ones.
		ofile = filename( c(  x@usedObj$lastGroup, method, "csv" ), opath )
		#print( paste( "Saving stats table as", file.path(opath,ofile)))
		if ( colnames(data)[1] == 'cor' ){
			data = cbind( 'Gene Name' = rownames(data), data )
		}
		utils::write.table( data, file= file.path(opath, ofile) , quote=F, sep=",", row.names=FALSE )

		mainOfile = x@usedObj$sessionRmdFiles[1]

		## we need a table to describe the grouping - INCLUDING COLOR INFORMATION
		## the VR ids are R ID's -1 (!!) make that clearly visible.
		## get the VR order:
		gInfo = groupingInfo(x)


		
		tableHTML = HTMLtable(gInfo)
		if ( ! empty(gInfo@linarObj) ){
			tableHTML = HTMLtable(gInfo@linarObj)
		}
		gname = gInfo@gname

		if ( !is.na( match( paste(gname, "pseudoBulk"), names(x@groupSelectedFrom)))){
			pseudoB_group = groupingInfo( x, paste(gname, "pseudoBulk") )
			if ( ! empty(pseudoB_group@linarObj) ){
				tab = table( pseudoB_group@linarObj@dat$col)
			}else {
				tab = table(as.numeric(pseudoB_group@grouping))
			}
			if ( ! empty(gInfo@linarObj) ){
				tmp = gInfo@linarObj
			}else {
				tmp = gInfo
			}
			tableHTML = HTMLtable(tmp,  
				list ( 
					'pseudo bulk samples [n]' = tab 
				) )
		}

		tableHTML = paste(tableHTML, "\n")
		## this need to become a relative path - relative to the final outfile

		if (length(grep('Time.group', x@usedObj$lastGroup)) > 0 ) {
			selF = filename(paste(sep=".", "TimeSelection", gInfo@gname, 'txt'))
			sesionFile = file.path(x@usedObj$sessionPath, selF )
			if ( ! file.exists( sesionFile )){
				exportSelection( gInfo@linarObj, sesionFile )
			}
		}else {
			if ( is.null(x@usedObj$SelectionFiles[[x@usedObj$lastGroup]] )){
				fname = file.path( x@outpath, x@usedObj$sessionName, paste(sep=".","selection", x@usedObj$lastGroup, "txt") )
				fname = stringr::str_replace_all( fname, '\\s+', '_')
				exportSelection( groupingInfo( x, x@usedObj$lastGroup), fname=fname )
				x@usedObj$SelectionFiles[[x@usedObj$lastGroup]] = fname
			}
			sesionFile = file.path( x@usedObj$sessionPath, basename( x@usedObj$SelectionFiles[[x@usedObj$lastGroup]] ) )
			if ( ! file.exists( sesionFile )){
				file.copy( x@usedObj$SelectionFiles[[x@usedObj$lastGroup]], sesionFile)
			}
		}

		if ( is.null(text)){
			text = ""
		}

		text = paste("\n", text, paste(
			"The statistical test is based on psedo bulk samples,",
			"not single cells as it has been shown that merging single",
			"cells into pseudo bulk samples improves the gene expression varianze",
			"which in turn increases the reliability of the statistical tests (<a href='https://www.nature.com/articles/s41467-021-25960-2#Sec10'>paper</a>).")
		)
		figures   = NULL
		figures2 = NULL

		if ( length(grep('Time.group', x@usedObj$lastGroup)) > 0 ){
			figures = drcFiles2HTMLlinear(x, gInfo, showIDs = showIDs )
		}else {
			figures = drcFiles2HTML(x, gInfo, showIDs = showIDs )
		}
		if ( length( gInfo@drc) > 1 ){
			## I do not support more than two here to ignore if it is more
			gInfo@drc = c( gInfo@drc[2], gInfo@drc[1])
			if ( length(grep('Time.group', x@usedObj$lastGroup)) > 0 ){
				figures2 = drcFiles2HTMLlinear(x, gInfo, showIDs = FALSE )
			}else {
				figures2 = drcFiles2HTML(x, gInfo, showIDs = TRUE )
			}
		}

		toppGF = filename( c(  x@usedObj$lastGroup,'toppClusterInput', "csv" ), opath )
		try( {
			toppgenes = getSigGenes( data, count=100, nameAdd = nameAdd)
			write.table( toppgenes, file=file.path(opath, toppGF), col.names=F, row.names=F, quote=F, sep="," )
		})

		content=paste( sep="\n", collapse = "\n",
				paste( "##", x@usedObj$lastGroup,"Statistical Result" ),
				"",text,"",
				tableHTML,
				"",
				"### Data",
				paste(sep="",  "<a href='",file.path( ".", x@usedObj$sessionName, 'tables',ofile),"' download>","This tab separated table </a>",
					" contains the statistical results for ",
				    "<a href='",file.path( ".", x@usedObj$sessionName, basename(sesionFile)),"' download>"," this selection file</a>", ".","\n\n",
				    "You can use <a href='",file.path( ".", x@usedObj$sessionName, 'tables',toppGF),"' download>this file</a> to create a pathway analysis using the ToppCluster web tool. "
				    ),
				""
		)
		if ( is.null(figures2) ) { #default
			content=paste( sep="\n", collapse = "\n", 
				content,
				paste(collapse = "\n", sep="\n",figures)
			)
		}
		else {
			figures = stringr::str_replace_all( figures, '###', '')
			figures2 = stringr::str_replace_all( figures2, '###', '')
			## remember drc names have been flipped
			content=paste( sep="\n", collapse = "\n", 
				content,
				paste( sep="",
					"<table><tr><th>drc dimensions [x,y]</th><th>", gInfo@drc[2],"</th><th>", gInfo@drc[1],"</th></tr>\n",
					"<tr><td> 1,2 </td><td>",figures[3],"</td><td>",figures2[3],"</td></tr>"
					)
			)
			if ( length(figures) + length(figures2) > 2 ) {
				content=paste( sep="\n", collapse = "\n", 
				content,
				paste( sep="",
					"<tr><td> 1,3 </td><td>",figures[7],"</td><td>",figures2[7],"</td></tr>\n",
					"<tr><td> 2,3 </td><td>",figures[11],"</td><td>",figures2[11],"</td></tr>\n",
					"</table>\n"
					)
				)
			}
			else {
				content=paste( sep="\n", collapse = "\n", 
					content, "</table>\n"
				)
			}
		}


		## the -log10 p value histogram
		if ( ! is.null(col) ){

			opath = file.path( x@outpath, x@usedObj$sessionName, 'png' )
			ofile = filename( c( 'hist',x@usedObj$lastGroup,method,"png"), opath )
			grDevices::png( file= file.path(opath, ofile), width=800, height=800)
			#browser()
			dat = -log10(as.numeric(data[,col]))
			# should not be used here as we lack the < 1 fraction of the plot due to the VR requirements
			#graphics::plot( data[,'logFC'], -log10(data[,'p.value']), main = paste( x@usedObj$sessionName, gname ), ylab='-log10(p.value)', xlab="logFC" )
			graphics::hist( dat[which(!is.na(dat))], main = paste( x@usedObj$sessionName, gname ), xlab="-log10(pvalue)"  )
			grDevices::dev.off()

			content = c( content, paste( sep="\n",
					paste("![](",file.path(x@usedObj$sessionName, 'png', ofile),")"),
					"" ))
		}
		
		x = storeLogContents( x, content, type='Stats') # in file renderFile.R
		id = length(x@usedObj$sessionRmdFiles)
		x = renderFile( x, id, type='Stats' )

	}
	invisible(x)
} )
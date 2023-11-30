#' Intern method to convert the object to a string for the show function.
#'
#' @name toString
#' @docType methods
#' @description summarize a cellexalLinear object as string
#' @param x the cellexalLinear object
#' @title stringify the linearSelection (print/show)
#' @export 
setMethod('toString', signature = c ('cellexalLinear'),
definition = function ( x ) {
	txt = paste(
		paste("An object of class", class(x),"with id", x@id,"\n" ),
		paste( 'with',nrow(x@dat),'linearly ordered points and', ncol(x@dat),' values.',"\n"),
		paste( 'The object is basis for the cellexalvrR grouping', x@gname),
		paste( 	"\nand is based on the selection", x@parentSelection)
	)
	if ( length(x@geneClusters) >0 ) {
		txt =paste ( txt, "\n",
			paste("It contains information about", length(x@geneClusters), 
				'Gene list(s):',"\n"),
		    paste( sep=",", collaspe=" ",names( x@geneClusters ))
		)
	}

	txt
}
)


#' @name show
#' @docType methods
#' @description  show the cellexalvrR
#' @param object the cellexalLinear object
#' @return nothing at all
#' @title show the object contents
#' @export show
setMethod('show', signature = c ('cellexalLinear'),
definition = function ( object ) {
	cat ( toString(object) )
}
)


#' @name empty
#' @docType methods
#' @description check if this object is empty
#' @param object the cellexalLinear object
#' @return true - is empty; false - contains data
#' @title show the object contents
#' @export empty
setGeneric('empty', ## Name
function ( object  ) { 
	standardGeneric('empty')
}
)
setMethod('empty', signature = c ('cellexalLinear'),
definition = function ( object ) {
	nrow(object@dat) == 0
}
)

# #' Method to copy a selection from cellexalObj to cellexalObj.
# #' This function makes sure that the linear grouping is fitting into the object.
# #' @name addSelection
# #' @docType methods
# #' @rdname addSelection
# #' @description add this cellexalLinear to the corresponding cellexal object
# #' @param x the cellexalLinear object
# #' @param cellexalObj the cellexalvrR object to add the linear selection as group
# #' @param upstreamSelection which group was the basis for this linearSelection?
# #' @title savely add a cellexalLinear object to a cellexalvR object
# #' @export 
# #if ( ! isGeneric('renew') ){
# setGeneric('addSelection', ## Name
# function ( x, cellexalObj, upstreamSelection=NULL  ) { 
# 	standardGeneric('addSelection')
# }
# )
# #}


# #' @rdname addSelection
# setMethod('addSelection', signature = c ('cellexalLinear', 'cellexalvrR'),
# definition = function ( x, cellexalObj, upstreamSelection=NULL ) {

# 	if ( is.null(upstreamSelection)){
# 		upstreamSelection = groupingInfo(cellexalObj , x@parentSelection)@gname
# 		if ( class(upstreamSelection)[1] == "cellexalvrR"){
# 			cellexalObj = upstreamSelection
# 			upstreamSelection <- groupingInfo( cellexalObj, cellidfile )
# 		}

# 	}
# 	id= (ncol(cellexalObj@userGroups) /2) + 1

# 	x@gname = paste( "Time.group", id, sep="." ) 
# 	if ( is.null( cellexalObj@usedObj$linearSelections )){
# 		cellexalObj@usedObj$linearSelections= list()
# 	}else{
# 		## Is this linearSelection a copy of an other?
# 		done=0
# 		for ( n in names(cellexalObj@usedObj$linearSelections)){
# 			if ( cellexalObj@usedObj$linearSelections[[n]]@id == x@id ) {
# 				x@gname = cellexalObj@usedObj$linearSelections[[n]]@gname
# 				cellexalObj@usedObj$linearSelections[[n]] = x
# 				cellexalObj@groupSelectedFrom[[ x@gname ]]@linarObj = x
# 				cellexalObj@usedObj$linearSelections[['lastEntry']] = x
# 				done=1
# 			}
# 		}
# 		if ( done ){
# 			return( invisible( cellexalObj) )
# 		}
# 	}
# 	cellexalObj@usedObj$linearSelections[['lastEntry']] = x
# 	cellexalObj@usedObj$linearSelections[[ x@gname ]] = x
# 	## I need to create a new one named 
# 	info = groupingInfo(cellexalObj, upstreamSelection )

# 	info@selectionFile = basename(paste( sep=".", cellexalObj@usedObj$SelectionFiles[[ upstreamSelection ]], 'time'))
# 	info@linarObj = x

# 	info@gname = x@gname

# 	cellexalObj@groupSelectedFrom[[ x@gname ]] = info

# 	m = match( rownames(x@dat), rownames(cellexalObj@drc[[x@drc[1]]]) )

# 	t1 = as.matrix(x@dat[,c('a','b','c')])

# 	t2 = as.matrix(cellexalObj@drc[[x@drc[1]]][m,1:3])

# 	colnames(t1) = colnames(t2)
# 	#rownames(t1) = rownames(t2)
# # 	if ( ! isTRUE( all.equal( t1, t2) ) ){
# # 		message("CRITICAL ERROR: drc models are not the same - if this is 'intentional' fixit by
#   # x@dat[,'a'] = t2[,1]
#   # x@dat[,'b'] = t2[,2]
#   # x@dat[,'c'] = t2[,3]
#   # t1 = t2
# # 			")
# # 		if(interactive()) { browser() }
# # 		if ( ! isTRUE( all.equal( t1, t2) ) ){
# # 			stop( "The drc models are not the same!")
# # 		}
# # 	}

# 	if ( length(which(is.na(m)))>0){
# 		stop("ERROR: This linearSelection describes cells that are not in the cellexalvrR object!")
# 	}

# 	m = match( rownames(x@dat), colnames(cellexalObj@data) )

# 	cellexalObj@userGroups[,x@gname] = NA
# 	cellexalObj@userGroups[m,x@gname] = as.vector(x@dat$time)

# 	cellexalObj@userGroups[,paste(x@gname, sep=" ", 'order')] = NA
# 	cellexalObj@userGroups[m,paste(x@gname, sep=" ", 'order')] = order( x@dat$time )

# 	#all.equal(cellexalObj@userGroups[m,paste(x@gname, sep=" ", 'order')],  order( x@dat$time ) )
# 	cellexalObj@usedObj$lastGroup = x@gname
# 	#browser()
# 	col = rep('gray80', ncol(cellexalObj@data))
# 	col[m] = as.vector(x@dat$col)
# 	#all.equal( rownames(cellexalObj@drc[[info@drc]])[m], rownames(x@dat))
# 	#rgl::open3d()
# 	#rgl::plot3d( cellexalObj@drc[[info@drc]], col=col)
# 	#rgl::points3d (cbind( x@dat[,c('a','b')], 'c'= rep(1, nrow(x@dat)) ), col= as.vector(x@dat$col))

# 	#rgl::plot3d( cellexalObj@drc[[info@drc]][ cellexalObj@userGroups[,paste(x@gname, sep=" ", 'order')],], col=wesanderson::wes_palette("Zissou1", nrow(cellexalObj@userGroups), type = "continuous") ) 

# 	invisible( cellexalObj)
# } )


#' The sanity check for a linear grouping.
#' If problems are detected the problems will be fixed.
#' @name checkTime
#' @rdname checkTime
#' @docType methods
#' @description checks for NA elements in the checkTime object and removes them
#' @param x the cellexalLinear object
#' @param cellexalObj an optional cellexalvrR object - if given ONLY the drc models are checked.
#' @title check a cellexalLinear's internals
#' @export 
#if ( ! isGeneric('renew') ){
setGeneric('checkTime', ## Name
function (x, cellexalObj=NULL) { 
	standardGeneric('checkTime')
}
)
#}


#' @rdname checkTime
setMethod('checkTime', signature = c ('cellexalLinear'),
definition = function (x, cellexalObj=NULL) {

	if ( nrow(x@dat) == 0 ){
		warning("empty object")
		return ("empty")
	}
	bad=which( is.na(x@dat$time))
	if ( length(bad) > 0 ){
		warning("Missing values detected in the linear order - dropping these")
		x@dat= x@dat[-bad,]
	}
	x@dat = x@dat[order(x@dat$time),]
	## onestly change the color NOW!
	ids = ceiling(seq( from=0, to=10,  length.out = nrow(x@dat) +1))
	ids = ids[-1]
	if ( length( x@id )== 0 ) {
		x@id = digest::digest( x@dat, algo="md5")
	}
	x@dat$col = factor(wesanderson::wes_palette("Zissou1", 10, type = "continuous")[ ids ], 
		levels= wesanderson::wes_palette("Zissou1", 10, type = "continuous") )

	if ( !is.null( x@geneClusters[['collapsedExp']] )){
		if ( ncol(x@geneClusters[['collapsedExp']] ) > 1000 ) {
			warning("Linear selection collapsedExp too much data!")
		}
	}
	if ( identical(x@gname, character(0)) ){
		if ( ! is.null(cellexalObj) ){
			x@gname = cellexalObj@usedObj$lastGroup
		}
		else {
			stop("The name of the timeline is missing!")
		}
	}
	invisible(x)
} )


#' @rdname checkTime
setMethod('checkTime', signature = c ('cellexalLinear', 'cellexalvrR'),
definition = function (x, cellexalObj=NULL) {

	error = NULL
	if ( is.null(cellexalObj@drc[[x@drc[1]]])){
		error = c( error, paste( sep="",
			"The basis drc for this linearSelection (", paste(names(cellexalObj@drc)),
			") is not part of this cellexal object" ))
	}else {
		mine = x@dat[,c('a','b','c')]
		ids = NULL
		if ( !is.null(rownames(cellexalObj@drc[[x@drc[1]]])) ){
			ids = match(rownames(x@dat), rownames(cellexalObj@drc[[x@drc[1]]]) )
		}else {
			ids = match(rownames(x@dat), colnames(cellexalObj@data) )
		}
		other = cellexalObj@drc[[x@drc[1]]][ids,1:3]

		if ( class(other)[1] == 'numeric'){
			other = data.frame (matrix( other, ncol=3))
			if ( any(is.na(ids)) ){
				browser(text="na's in the ids - how could that happen?!")
			}
			rownames(other) = colnames(cellexalObj@data)[ids]
		}
		colnames(mine) = c('x', 'y','z')
		colnames(other) = c('x','y','z')
		if ( !isTRUE( all.equal( as.matrix(mine), as.matrix(other))) ) {
			x@dat = x@dat[which(!is.na(ids)),]
		}
	}

	if ( ! is.null(error) ) {
		message(  paste(collapse="\n\r", sep=" ", c( "checkTime",error ) ) )
		x@error = paste(collapse="\n\r", sep=" ", c( "checkTime",error ) )
	}
	invisible(x)
} )


#' returns a color vector based on the intern col column and the cell names
#' Cells that are not part of this selection will get gray(0.6) as color.
#' @name color
#' @rdname color
#' @docType methods
#' @description return the color in exactly the right format to color the given cell names
#' @param x the cellexalLinear object
#' @param names the cells to color with the pseudo time color
#' @title return a pseudo time color vector for a given cell names list
#' @export 
#if ( ! isGeneric('renew') ){
setGeneric('color', ## Name
function (x, names) { 
	standardGeneric('color')
}
)
#}


#' @rdname color
setMethod('color', signature = c ('cellexalLinear'),
	definition = function (x, names) {
	col = rep( gray(0.6), length(names) )
	m = match( names, rownames(x@dat) )
	col[which(!is.na(m))] = as.vector(x@dat$col[m[which(!is.na(m))]])
	col
} )

#' @name compactTime
#' @docType methods
#' @description create a global gene z-score and merged virtual samples over the linearSelection
#' @param cellexalObj if x is a cellexalLinear object this is necessary to create the zscored matrix.
#' @title merge cells based on linear order
#' @export 
#if ( ! isGeneric('renew') ){
setGeneric('compactTime', ## Name
	function ( cellexalObj ) { 
		standardGeneric('compactTime')
	}
)
#}


#' @rdname compactTime
setMethod('compactTime', signature = c ('cellexalvrR'),
definition = function (  cellexalObj ) {
	## now I need to check which linearSelections I have

	for (tName in names(cellexalObj@usedObj$linearSelections)) {
		if ( tName != "lastEntry" && 
			is.null( cellexalObj@usedObj$linearSelections[[tName]]@geneClusters[['collapsedExp']])){
			time = cellexalObj@usedObj$linearSelections[[tName]]
			n = nrow(time@dat)
			loc = reduceTo( cellexalObj, what='col', to=rownames(time@dat))
			if ( n > 1000 ){
				ids = rep( 1:1000, floor(n/1000))
				ids = c( ids, sample( 1:1000, n%%1000))
				ids = sort(ids)
				B = FastWilcoxTest::collapse( loc@data, ids, 2 )
				B <- Matrix::Matrix(B, sparse = TRUE)
				rownames(B) = rownames(cellexalObj@data)
				colnames(B) = 1:1000
				cellexalObj@usedObj$linearSelections[[tName]]@geneClusters[['collapsedExp']] = B
			}else {
				cellexalObj@usedObj$linearSelections[[tName]]@geneClusters[['collapsedExp']] = loc@data
			}
		}
	}

	invisible(cellexalObj)
})

 
#' This function should be run on the main cellexalObj including all data.
#' It will create (for a subset of genes) a z-scored dataset and will then 
#' sum up the expression in the linear orientation to represent 1000 'cells' in the linearSelection.
#' @name compactTimeZscore
#' @docType methods
#' @description create a global gene z-score and virtual samples over the linearSelection
#' @param x the cellexalLinear object
#' @param deg.genes a list of genes  cluster the genes for (list)
#' @param cellexalObj if x is a cellexalLinear object this is necessary to create the zscored matrix.
#' @title merge and z-score gene expression for a cellexalLinear object
#' @export 
#if ( ! isGeneric('renew') ){
setGeneric('compactTimeZscore', ## Name
function ( x, deg.genes,  cellexalObj ) { 
	standardGeneric('compactTimeZscore')
}
)
#}

#' @rdname compactTimeZscore
setMethod('compactTimeZscore', signature = c ('cellexalLinear', 'character', 'cellexalvrR'),
definition = function ( x, deg.genes, cellexalObj ) {

	cellexalObj = addSelection( cellexalObj, x) # just make sure all is OK
	
	cellexalObj = compactTime ( cellexalObj )

	cellexalObj = reduceTo( cellexalObj, what='row', to=deg.genes )

	collapsedData = NULL
	collapseNames = NULL
	for ( tName in names(cellexalObj@usedObj$linearSelections)) {
		if ( tName != "lastEntry"){
			collapsedData =cbind(collapsedData, cellexalObj@usedObj$linearSelections[[tName]]@geneClusters[['collapsedExp']] )
			collapseNames = c( collapseNames, rep(cellexalObj@usedObj$linearSelections[[tName]]@gname
				, ncol(cellexalObj@usedObj$linearSelections[[tName]]@geneClusters[['collapsedExp']]) ))
		}
	}
	colnames( collapsedData ) = collapseNames
	B = FastWilcoxTest::ZScoreAll( collapsedData )
	colnames(B) = collapseNames
	rownames(B) = rownames(cellexalObj@data)
	ok = which(! is.na(match(  colnames(B), x@gname)))
	B = B[,ok]
	n = nrow(x@dat)
	toPlot = data.frame(time= x@dat[,'time'], col = x@dat[,'col'] )
	if (n > 1000) {
		ids = rep( 1:1000, floor(n/1000))
		ids = c( ids, sample( 1:1000, n%%1000))
		ids = sort(ids)
		toPlot = data.frame(
	 		time=unlist(lapply( split( x@dat[,'time'], ids ), mean)), 
	 		col= unlist(lapply( split( as.vector(x@dat[,'col']), ids ), 
	 			function(x) { t=table(x); names(t)[which(t==max(t))[1]]})) 
	 	)
	}
	cbind( toPlot, t(B))
})


#' This is a none VR method to compare two different linearSelections.
#' The main usage for this function is to run the same genes trough trough one linearSelection 
#' that has been split into two sub-sections like young and old or teated and untreated.
#' This function only allows to compare two cellexalLinear objects.
#' This function highligts which genes follow different expression patterns in both linearSelections.
#'
#' @name compareGeneClusters
#' @docType methods
#' @description return the color in exactly the right format to color the names
#' @param x the cellexalLinear object
#' @param other the other linearSelection(s) (the parentSelection has to match)
#' @param cellexalObj the cellexalObj to add the report to.
#' @param altGroupNames you can give alternative group names here
#' @param color a vector of colors - one for each group
#' @param GOIs a different group of genes - might need to re-run the analysis
#' @title compare two cellexalLinear objects (interactive mode only)
#' @export 
#if ( ! isGeneric('renew') ){
setGeneric('compareGeneClusters', ## Name
function (x, other, cellexalObj, altGroupNames=NULL, color=NULL, GOIs=NULL) { 
	standardGeneric('compareGeneClusters')
}
)
#}


#' @rdname compareGeneClusters
setMethod('compareGeneClusters', signature = c ('character', 'character', 'cellexalvrR' ),
definition = function (x, other, cellexalObj, altGroupNames=NULL, color=NULL, GOIs=NULL ) {
	compareGeneClusters( x=getLinearSelection(cellexalObj,x), other=c(getLinearSelection(cellexalObj, other)),
		cellexalObj=cellexalObj, altGroupNames=altGroupNames, color=color )
})

#' @rdname compareGeneClusters
setMethod('compareGeneClusters', signature = c ('cellexalLinear', 'cellexalLinear', 'cellexalvrR' ),
definition = function (x, other, cellexalObj, altGroupNames=NULL, color=NULL, GOIs=NULL ) {
	compareGeneClusters( x=getLinearSelection( cellexalObj, x), other=c(getLinearSelection(cellexalObj, other@gname)),
		cellexalObj=cellexalObj, altGroupNames=altGroupNames, color=color )
})

#' @rdname compareGeneClusters
setMethod('compareGeneClusters', signature = c ('cellexalLinear', 'list', 'cellexalvrR' ),
definition = function (x, other, cellexalObj, altGroupNames=NULL, color=NULL, GOIs=NULL ) {
	compareGeneClusters( x=x@gname, other=c(getLinearSelection(cellexalObj, other)),
		cellexalObj=cellexalObj, altGroupNames=altGroupNames, color=color )
})

#' @rdname compareGeneClusters
setMethod('compareGeneClusters', signature = c ('cellexalLinear', 'list', 'cellexalvrR' ),
definition = function (x, other, cellexalObj, altGroupNames=NULL, color=NULL, GOIs=NULL ) {
	
	if ( is.character( x) ){
		x= getLinearSelection(cellexalObj, x)
	}

	parent = x@parentSelection

	times = list()
	for ( time in c(x, other)){
		if ( is.character(time) ){
			time = getLinearSelection(cellexalObj, time)
		}
		if ( parent != time@parentSelection){
			#stop("the other object is not from the same parent selection - stop")
		}
		if ( class(time) == "cellexalGrouping" ){
			time = time@linarObj
		}
		if ( length(time@geneClusters) == 0 ){
			stop( paste( sep="\n",
			"Either the one or the other objects' geneClusters == 0",
		 	"For all linearSelections the function 'createReport' has to be run first.", paste("Error in", time@gname)) )
		}
		times = c(times, time)
	}


	if ( is.null(color) ) {
		color= grDevices::rainbow(length(times))
	}
	figTab = NULL
	if ( is.null(altGroupNames) ) {
		altGroupNames = unlist( lapply( times, function(time) { time@gname}) )
	}
	else {
		## missusing the error now.
		tmp = x@error
		for ( i in 1:length(times)) {
			times[[i]]@error = altGroupNames[i]
		}
		altGroupNames =  unlist( lapply( times, function(time) { 
			paste(sep="", time@error, " [", time@gname ,"]" ) }) )
		for ( i in 1:length(times)) {
			times[[i]]@error = tmp
		}
	}

	header= ""
	body= ""
	text = "## Comparison between the gene clusters of linearSelection"

	for ( i in 1:length(times)){
	 	header = paste(sep="",collapse="", header, "<th>",altGroupNames[i],"</th>")
	 	body   = paste(sep="",collapse="", body  , "<td>![](",
	 		R.utils::getRelativePath( times[[i]]@geneClusters[[times[[i]]@gname]]$figure, cellexalObj@outpath),
	 		")</td>"
	 		)
	 	if ( i ==1 ){
	 		text=paste(text, altGroupNames[i])
	 	}else if(i == length(times)){
	 		text=paste(text, "and", altGroupNames[i], "\n\n")
	 	}else {
	 		text=paste(sep="",collapse="",text, ", ", altGroupNames[i])
	 	}
	}

	figTab = paste('<table><tr>',header,
	 	'</tr><tr>',body,'</tr></table>', "\n\n")
	text = paste(text, 
   	paste("All linearSelections have been run based on the selection", 
   		x@parentSelection,
    	"\n\n")
   )
   text = paste(text,figTab)

   
   # checkGeneList = function( mine , cmpTo) {
   # ## return the other group with highest overlap and the genes that do not overlap
 	 #  dat = lapply( cmpTo,
   #         function( mine, other){
   #              m = match(mine, other)
   #              mine[which(!is.na(m))]
   #         }, mine )
   # 	dat
   # }

   
   genes = as.vector(unlist(x@geneClusters[[x@gname]]['clusters'][['clusters']]))
   df = matrix (ncol=length(altGroupNames), nrow=length(genes))
   rownames(df) = genes
   colnames(df) = unlist( lapply(times, function(time){time@gname}))
   df = data.frame(df)
   for ( time in times ){
   	clusters = time@geneClusters[[time@gname]]['clusters'][['clusters']]
   	 for ( i in 1:length(clusters)) {
   	 	for( gn in clusters[[i]] ){
   	 		df[gn, time@gname ] = i
   	 	}
   	 }
   }
   
   if ( length(which(is.na(df[]))) > 0 ){
   	stop( "Please analyze the timelines using the same list of genes all the time")
   }
   df$tag = apply(df,1, paste, collapse="-")
   #df= df[order(df$tag),]

   ## now each different combo of rowInfo needs to be described.
   
   section = paste( collapse="\n\n", sep="\n\n", unlist(lapply( unique(df$tag) , function(tag){
   		text = paste(sep="","\n\n### Genes in the timeline comparison group ", tag,".")
   		## here we create one section of one tag like 1-4-6 meanung time[1] group 1 time[2] group4 and time[3] group 6
   		genes = rownames( df)[which(df$tag == tag)]
   		## each of these genes has the same info
   		## hence the group color circles for all genes are the same and can be stated now:
   		header=""
   		body=""
   		circles = NULL

   		for (i in 1:length(times) ){
   			groupID = df[genes[1], i]
   			circles = c( circles, R.utils::getRelativePath( 
   				paste( sep=".",times[[i]]@geneClusters[[times[[i]]@gname]]$figure,
   					groupID,'svg'), 
   				cellexalObj@outpath) )

   		}

 		## the (hidden) gene names
 		text = paste( text, sep="\n", collapse="\n",
			md_gene_links ( sort(genes), label="Click to expand lexically sorted" ) )
   			text = paste( text,  
   			md_gene_links ( rev(genes), label="Click to expand in heatmap order" ), "\n\n" )
   		
 		## and now the heatmaps for this set of genes.
 		for (i in 1:length(times) ){
 			groupID = df[genes[1], i]
 			of = paste(sep="_", "directComp",  times[[i]]@gname, tag, length(genes), "heatmap" )
			of2 = file.path( cellexalObj@usedObj$sessionPath, 'png', of )
			
			groupCol = paste( times[[i]]@gname, 'groupColors','png', sep=".")

 			text = paste( text, sep=" ",collapse="", "\n\n#### Expression in", 
					altGroupNames[i],"and trajectory",groupID," / <img src='",circles[i],"' height='20'/>", "\n\n",
					paste("![](",correctPath(groupCol, cellexalObj ),")\n"),
					plotHeatmap_rmd( 
						x=t(times[[i]]@geneClusters[[times[[i]]@gname]]$matrix[,genes]), 
						color=color[i] ,
						ofile = of2,
						cellexalObj = cellexalObj
					), "\n\n")
 			#print(paste("DEBUG:",of,"genes n=", length(genes)) )
 		}
   		return ( paste(text, '\n', sep="") );
   	})))


   cellexalObj = storeLogContents(cellexalObj, paste( text,section),  type="GeneClusterComparison" )
   id = length(cellexalObj@usedObj$sessionRmdFiles)
	cellexalObj = renderFile(cellexalObj, id, type = "GeneClusterComparison")
	cellexalObj
}

)



#' collapse the linear selection to a max of 1000 entries rendering the 
#' linearSelection incompatible with cellexalvrR
#' but allowing the correlation of collapsed expression to the pseudo time.
#' @name collapseTime
#' @docType methods
#' @description return the color in exactly the right format to color the names
#' @param x the cellexalLinear object
#' @param to the max amnount of pseudo time points
#' @title sum up cells based on the pseudo time
#' @export 
#if ( ! isGeneric('renew') ){
setGeneric('collapseTime', ## Name
function (x, to =1000) { 
	standardGeneric('collapseTime')
}
)
#}


#' @rdname collapseTime
setMethod('collapseTime', signature = c ('cellexalLinear' ),
definition = function (x, to=1000 ) {
	n= nrow(x@dat)
	if ( n > to ) {
		ids =sort(rep( 1:to, floor(n/to) ))
		if ( n%%to > 0 ){
			ids = sort(c(ids, sample(1:to, n%%to)) )
		}
		tmp = x@dat[ match(1:to, ids),]
		tmp[,'time'] = sapply( split( as.vector(x@dat[,'time']), ids ), 
			function(dat) { mean(dat) })
		tmp[,'col']= unlist(lapply( split( as.vector(x@dat[,'col']), ids ), 
	 			function(dat) { t=table(dat); names(t)[which(t==max(t))[1]]})) 

		x@dat = tmp
	}
	x

})


#' This report is the main go to analysis for a linearSelection.
#' It will produce gene clusters and plot the mean expression of these genes clusters as beautiful xy plots.
#' It also populates the CellexalTime geneClusters slot with a list entries containing
#' the gene clusters (clusters), the max 1000 cells zscored data (matrix) and the location of the main figure (png file path; figure)
#'
#' After this function two cellexalLinear objects can be compared to another using the function compareGeneClusters.
#'
#' @name createReport
#' @docType methods
#' @description 3D plot using linear order colors
#' @param x the cellexalLinear object or a linear grouping name
#' @param cellexalObj the object to get the data from
#' @param info a cellexalGrouping or grouping name
#' @param deg.genes a list of genes to create the report for
#' @param num.sig number of signififcant genes (default 250)
#' @title create a report for one cellexalLinear object (interactive mode only)
#' @export 
#if ( ! isGeneric('createReport') ){
setGeneric('createReport', ## Name
function ( x, cellexalObj, info, deg.genes=NULL, num.sig=250) { 
	standardGeneric('createReport')
}
)
#}


#' @rdname createReport
setMethod('createReport', signature = c ('character', 'cellexalvrR', 'character'),
definition = function ( x, cellexalObj, info, deg.genes=NULL, num.sig=250 ) {
	createReport ( x =getLinearSelection(cellexalObj, x), cellexalObj=cellexalObj, info=groupingInfo(x,info), deg.genes=deg.genes )
})

#' @rdname createReport
setMethod('createReport', signature = c ('cellexalLinear', 'cellexalvrR', 'character'),
definition = function ( x, cellexalObj, info, deg.genes=NULL, num.sig=250 ) {
	info = groupingInfo(cellexalObj, info )
	createReport ( x =x, cellexalObj=cellexalObj, info=info, deg.genes=deg.genes )
})

#' @rdname createReport
setMethod('createReport', signature = c ('cellexalLinear', 'cellexalvrR', 'cellexalLinear'),
definition = function ( x, cellexalObj, info, deg.genes=NULL, num.sig=250 ) {
	info = groupingInfo(cellexalObj, info@gname )
	createReport ( x =x, cellexalObj=cellexalObj, info=info, deg.genes=deg.genes )
})

#' @rdname createReport
setMethod('createReport', signature = c ('cellexalLinear', 'cellexalvrR', 'cellexalGrouping'),
definition = function ( x, cellexalObj, info, deg.genes=NULL, num.sig=250 ) {

#print( paste( "I am in cellexalLinear::createReport and was called from here:", deparse(sys.calls()[[sys.nframe()-2]]) ) )

	text = NULL
	if ( is.null(deg.genes)){
		deg.genes = cellexalObj@usedObj$deg.genes
		message('using the deg.genes stored in the object')
	}else {
		cellexalObj@usedObj$deg.genes = deg.genes
		message('using the deg.genes provided by the user')
	}
	print ( paste("I am analyzing", length(deg.genes),"deg.genes"))

	bad= which(is.na(match(deg.genes, rownames(cellexalObj@data)) ))
	if ( length(bad) > 0) {
		bad.genes = paste( collapse=", ", deg.genes[bad] )
		deg.genes = deg.genes[-bad]
		text = paste(collapse=" ", sep=" ",
			"From the requested gene list the gene(s)", 
			bad.genes, 
			"is/are not expressed in at least 1% of the cells." 
			)
	}
	#ret = list( genes = split( names(gr), gr), ofile = ofile, pngs = pngs )

	ps = cellexalObj@usedObj$sigGeneLists$lin[[x@gname]]
	if ( is.null(ps)) {
		message("the linear statistics table is NULL!")
		cellexalObj = createStats( x, cellexalObj, num.sig=num.sig )
		cellexalObj@usedObj$deg.genes = deg.genes
		ps = cellexalObj@usedObj$sigGeneLists$lin[[x@gname]]

	}
	o = order(ps[,'p.value'])
	ps = ps[o,]
	cellexalObj@usedObj$lastGroup = info@gname

	## add the plots to the log
	#try({
		#print( "simplePlotHeatmaps" )
		
		ret = simplePlotHeatmaps(
			cellexalObj, 
			info= info,  
			fname=file.path( cellexalObj@usedObj$sessionPath,'png', info@gname ) 
		)
		x@geneClusters[[info@gname]] = list( 
			clusters= ret$genes, 
			matrix = ret$mat, 
			figure= ret$ofile,
			groupColors = ret$groupColors, ## a legend for the heatmaps
			smoothedClusters = ret$smoothedClusters,
			MaxInCluster = ret$MaxInCluster
		)
		#print( "logLinear selection" )
		cellexalObj = logTimeLine( cellexalObj, ps, ret$genes, 
			info = x, 
			png = c( ret$ofile, ret$pngs ),
			timeInfo = x, 
			text= paste(text, ret$error, sep=" ", collapse=" ")
		) 
		#print( "finish" )
	#} )	

	#	print ( paste("4: createReport: The dims of the cellexalObj:", paste(collapse=", ", dim(cellexalObj@data))))

	invisible( cellexalObj )
	} )


#' Calculates linear peason statistics for the linearSelection and the 
#' expression data in the cellexalvrR object.
#' @name createStats
#' @docType methods
#' @description Create statistics for the linearSelection.
#' @param x the object
#' @param cellexalObj the object to get the data from
#' @param num.sig the amountof genes to return as top genes (default 250)
#' @param p.cut use a p value cut of instead of a number of significant genes
#' @param bulkStats create bulk stats even if the total number of cells is below 1000?
#' @title linear correlation between the pseudo time and the gene expression
#' @export 
#if ( ! isGeneric('createStats') ){
setGeneric('createStats', ## Name
function ( x, cellexalObj, num.sig=250, p.cut = NULL, bulkStats=FALSE ) { 
	standardGeneric('createStats')
}
)
#}

#' @rdname createStats
setMethod('createStats', signature = c ( 'character', 'cellexalvrR' ),
	definition = function ( x, cellexalObj, num.sig=250, p.cut = NULL, bulkStats=FALSE ) {
		createStats(x= getLinearSelection(cellexalObj, x ), cellexalObj=cellexalObj, num.sig=num.sig, p.cut=p.cut )
})

#' @rdname createStats
setMethod('createStats', signature = c ( 'cellexalLinear', 'cellexalvrR' ),
	definition = function ( x, cellexalObj, num.sig=250, p.cut = NULL, bulkStats=FALSE ) {

	#cellexalObj = addSelection( x, cellexalObj, info@gname)
	# focus on our data only
	cellexalObj@usedObj$lastGroup = x@gname
	if (  is.null(cellexalObj@usedObj$sigGeneLists$lin[[x@gname]])) {
			## otherwise the stats have already been calculated and do not need to be re-run.
		loc = reduceTo(cellexalObj, what='col', to=rownames(x@dat) )
		# get rid of genes not expressed in at least 1% of the cells
		nCells = FastWilcoxTest::ColNotZero( Matrix::t( loc@data ) )
		OK = which( nCells / ncol(loc@data)  > .01 )
		loc = reduceTo(loc, what='row', to = rownames(loc@data)[OK]  )
		

		## I want 1000 pseudo bulk samples here!
		if ( ncol(loc@data) > 1000 ){
			cellexalObj@usedObj$pseudoBulk = TRUE

			toGroups = 1000
			n = ncol(loc@data)
			ids = rep( 1:toGroups, floor(n/toGroups))
			ids = c( ids, sample( 1:toGroups, n%%toGroups))
			ids = sort(ids)

			bulk = reduceTo( loc, what='col', to= colnames(loc@data)[match(unique(ids), ids)])
			cn = colnames( bulk@data )
			rn = rownames( bulk@data )
			bulk@data = Matrix(FastWilcoxTest::collapse( loc@data, ids, type=2 ), sparse=TRUE) ## mean expression
		
			colnames( bulk@data ) = cn
			rownames( bulk@data ) = rn
			cellexalObj@usedObj$pseudoBulk = TRUE
			cellexalObj@groupSelectedFrom[[paste(x@gname, 'pseudoBulk')]] = groupingInfo( bulk, x@gname )

		}
		else if ( bulkStats ) {
			## oops with the new pseudo bulk stuff we might want to still collapse the data?!
			bulk = pseudoBulk( x, cellexalObj, maxBulks = 1000 )
			warning( "Too little data to run pseudo bulk analysis - forced collapse")
			cellexalObj@usedObj$pseudoBulk = TRUE
			cellexalObj@groupSelectedFrom[[paste(x@gname, 'pseudoBulk')]] = groupingInfo( bulk, x@gname )

		}
		else {
			warning( "Too little data to run pseudo bulk analysis")
			bulk = loc
		}
		## run stats (pearson linear correlation directly on sparse matrix)

		save( bulk, file= file.path( bulk@usedObj$sessionPath, paste(sep=".", "PseudoBulk", x@gname,".RData" ) ) )
		toGSEA( bulk, 
			ofile=file.path( bulk@usedObj$sessionPath, paste(sep=".", "PseudoBulk", x@gname,".RData" ) ),
			x@gname
		)
		m = match( colnames(bulk@data), rownames(x@dat))

		ps = FastWilcoxTest::CorMatrix_N(  
		#	loc@data[, rownames(x@dat) ], 
		#	x@dat$time 
			bulk@data,
			x@dat$time[m]
			#1:ncol(loc@data)
		)
		rownames(ps) = rownames(bulk@data)
		addPval = function(dat){ 
			if( dat[2] < 3){
				1.1
			}
			else { 
				2* min( stats::pt( dat[3], dat[2]-2,lower.tail = TRUE ), 
				stats::pt( dat[3], dat[2]-2,lower.tail = FALSE ) )
			}
		}

		ps = cbind(ps, apply( ps, 1, addPval ) )
		colnames(ps) = c('cor', 'n', 't', 'p.value')

		ps[which(is.na(ps[,1])),1] = 0
		o = order(ps[,'cor'])
		ps = ps[o,]

		n = round( num.sig / 2)
		cellexalObj@usedObj$deg.genes = c(rownames(ps)[1:n], rev(rownames(ps))[n:1] )

		## could I do something useful here using the convolute?
		m = match( rownames(x@dat), colnames(cellexalObj@data))
		filter = rev(rep(1, 10) / 10)
		bulk = reduceTo( bulk, what='row', to = cellexalObj@usedObj$deg.genes )

		conv = apply(bulk@data, 1, function(da){
				conv = stats::convolve( da, filter, type='filter' )
			 (conv -mean(conv)) / stats::sd(conv)
			} )
		info = groupingInfo( cellexalObj, x@gname )
		info@linarObj = x
		gr = clusterGenes( t(conv),deg.genes=NULL, info=info)
		cellexalObj@usedObj$deg.genes = cellexalObj@usedObj$deg.genes[order(gr$geneClusters)]

		#graphics::image(conv[,order(gr$geneClusters)], col=gplots::bluered(40))
		#graphics::image(t(as.matrix(cellexalObj@data[cellexalObj@usedObj$deg.genes,m])), col=gplots::bluered(40))

		maxpos = function( da ) {
			rollSum = da
			rollSum[(length(da)-5):length(da)] = 0
			for ( i in 1:(length(da)-5)){
				rollSum[i:(i+5)] = mean(da[i:(i+5)])
			}
			which( rollSum == max(rollSum)) [1]
		}
		maxes = apply( conv,1, maxpos)

		if ( ! is.null(p.cut)){
			cellexalObj@usedObj$deg.genes = rownames(ps)[which( ps[,'p.value'] < p.cut ) ]
		}
		if ( is.null( cellexalObj@usedObj$linearSelections)) {
			cellexalObj@usedObj$linearSelections = list()
		}

		cellexalObj@usedObj$sigGeneLists$lin[[x@gname]] = ps
	}
	## the stats have already been run and all is OK.

	cellexalObj@usedObj$deg.genes = cellexalObj@usedObj$deg.genes[which(!is.na(cellexalObj@usedObj$deg.genes))]
	
	cellexalObj= logStatResult ( cellexalObj, method ='Linear', data=cellexalObj@usedObj$sigGeneLists$lin[[x@gname]], col='p.value'	 )

	invisible( cellexalObj )
} )


 
#' This function utilizes slingshot to identify the longest possible pseudo 
#' linearSelection for this selection.
#' In short the drc model of the selected genes is clustered using kmeans
#' and the first and last selected cell is used to determine the start and end points of the
#' anlysis in the drc model.
#' Subsequently slignshot is used to calculate the pseudo time over the selection.
#' This function will create the same linearSelection if the same cells are used as input.
#' @name createTime
#' @docType methods
#' @description calculate pseudo time based on the internal table a b and c columns
#' @param x the object
#' @param parentSelection the selection name this linear selection bases on.
#' @title create the pseudo time / linear selection for the selection 
#' @export
#if ( ! isGeneric('createTime') ){
setGeneric('createTime', ## Name
function ( x, parentSelection=NULL ) { 
	standardGeneric('createTime')
}
)
#}

#' @rdname createTime
setMethod('createTime', signature = c ('cellexalLinear'),
definition = function ( x, parentSelection=NULL ) {
	if ( is.null(parentSelection) && length(x@parentSelection)==0 ){
		stop( "cellexalLinear::createTime needs to know the parentSelection")
	}
	if ( !is.null( parentSelection )){
		x@parentSelection = parentSelection@gname
	}
	
	dat = cbind( x@dat$a, x@dat$b )
	colnames(dat) = c('a','b')
	if ( ! var(as.vector(x@dat$c)) == 0 ){
		dat = cbind( dat, x@dat$c )
		colnames(dat) = c('a','b','c')
	}
	mode(dat) = 'numeric'
	rownames = rownames(dat) = rownames(x@dat)
	bad= which(apply( dat,1, function(d) { all(is.na(d))}))
	if ( length(bad) > 0 ) {
		message( "pseudotimeTest3D - There are NA values in the dat matrix!")
		if ( interactive() ) {browser()}
		dat = dat[-bad,]
		rownames= rownames[-bad]
	}

	opt = optGroupCountKmeans( dat )
	group = stats::kmeans( dat , opt )
	dist_of_centers = NULL

	if ( ncol(dat) == 2) {
		dist_of_centers = FastWilcoxTest::eDist3d( group$centers[,'a'], group$centers[,'b'], rep(0, length(group$centers[,'b'])), 0 )
	}
	else {
		dist_of_centers = FastWilcoxTest::eDist3d( group$centers[,'a'], group$centers[,'b'], group$centers[,'c'], 0 )
	}
	end = which( dist_of_centers == max(dist_of_centers))

	if ( length(which(is.na(dat))) > 0 ){
		message( "pseudotimeTest3D - There are NA values in the dat matrix!")
		if ( interactive() ) {browser()}
	}
	sling = NULL
	#try( {
	sling = {
		#if ( ! interactive() ) {
		#	stop("This should not be applied here!")
		#	setTimeLimit(220)#after 120 sec this has failed - get a more simple selection
		#}
		slingshot::slingshot(dat, group$cluster, 
		start.clus= group$cluster[which(parentSelection@order == 1)], 
		end.clus = end  ) ## assuming that the first cell selected should also be in the first cluster...
	}
	#})
	if ( is.null(sling)) {
		stop("Slingshot has not returned a linearSelection in time - please select a more linear set of cells")
	}
	slingTime = slingshot::slingPseudotime( sling )


	## I am interested in the longest slope
	use = 1
	if ( ncol(slingTime) > 1){
		tmp= apply( slingTime,2, function(x){ length(which(! is.na(x))) } )
		use = which(tmp == max(tmp))
	}
	if ( length(which(is.na(slingTime[,use]))) > 0 ){
		print("The linearSelection could not reach all cells - possibly worth a debug session")
	}
	o = order( slingTime[,use])
	
	x@dat$time = slingTime[,use]
	x@dat$order = o

	## curves broken in 2.0 not important for VR Ignore!
	if ( FALSE ){
		if ( ! is.na(match ( 'curves',slotNames(sling))) ){
			curves = sling@curves
		}else{
			curves = sling@metadata$curves
		}
		x@dat$x = curves[[use]]$s[,1]
		x@dat$y = curves[[use]]$s[,2]
		if ( var(x@dat$c) == 0 ){
			x@dat$z = rep(0, nrow(slingTime))
		}
		else{
			curves[[use]]$s[,3]
		}
	}
	if ( ! is.null(parentSelection)){
		x@gname = parentSelection@gname
	}
	#plotTime(checkTime(x))
	checkTime(x) ## adds the color info
}
)



#' @name exportSelection
#' @docType methods
#' @description Export this linear selection as CellexalVR selection file
#' @param x the object
#' @param fname the file to write the info to
#' @title export a cellexalLinear object for CellexalVR
#' @export 
#if ( ! isGeneric('exportSelection') ){
	setGeneric('exportSelection', ## Name
		function (x, fname) { 
		standardGeneric('exportSelection')
	}
	)
#}


#' @rdname exportSelection
setMethod('exportSelection', signature = c ('cellexalLinear'),
	definition = function (x, fname) {

	dat = x@dat[order(x@dat$time),]
	d = cbind( rownames(dat), as.vector(dat$col), rep( x@drc[1] , nrow(dat) ), as.numeric( dat$col )  )
	utils::write.table( d, col.names=F, row.names=F, quote=F, sep="\t", file= fname)

	f2 = paste( sep=".",fname,'points')

	d = cbind( rownames(dat), dat$a, dat$b, dat$c  )
	utils::write.table( d, col.names=F, row.names=F, quote=F, sep="\t", file=f2)

	invisible(x)
} )


#' A log specific function returning a html formate table describing a cellexalvrR grouping.
#'
#' @name HTMLtable
#' @docType methods
#' @description Create a html table that descibes a grouping (multi group or pseudo time)
#' @param x the cellexalLinear object
#' @param addCol a list with colors as names that describes one additional column of data (default NULL)
#' @title log specififc - create an HTML summary table for this selection
#' @export 
#if ( ! isGeneric('HTMLtable') ){
	setGeneric('HTMLtable', ## Name
	function (x, addCol=NULL) { 
		standardGeneric('HTMLtable')
	}
	)
#}


#' @rdname HTMLtable
setMethod('HTMLtable', signature = c ('cellexalLinear'),
definition = function (x, addCol=NULL) {

	cellCount = table(x@dat[,'col'])
	tableHTML = paste( sep="\n",
			"### group information table",'',
			"<p> This is information on a linearSelection - VR and R ids are not set at the moment<p>",
		'<table>',
		'  <tr><th>Color</th><th>HTML tag</th><th>cell count [n]</th></tr>',

		paste(collapse="\n",
			sapply( dimnames(cellCount)[[1]], function(color){
			paste(sep="",
				'<tr><td style="background-color:', 
				color,'"',
				"></td><td>",
				color,"</td><td>",
				cellCount[match(color, dimnames(cellCount)[[1]])], "</td><td></tr>"
					)
				}))
			, '</table> '
	)

	if ( ! is.null(addCol) ){
		tableHTML = paste( sep="\n",
			"### group information table",'',
			"<p> This is information on a linearSelection - VR and R ids are not set at the moment<p>",
		'<table>',
		paste( sep="",'  <tr><th>Color</th><th>HTML tag</th><th>cell count [n]</th><th>',names(addCol)[1],'</th></tr>'),

		paste(collapse="\n",
			sapply( dimnames(cellCount)[[1]], function(color){
			paste(sep="",
				'<tr><td style="background-color:', 
				color,'"',
				"></td><td>",
				color,"</td><td>",
				cellCount[match(color, dimnames(cellCount)[[1]])], "</td><td>",addCol[[1]][color],"</td></tr>"
					)
				}))
			, '</table> '
	)


	}



	tableHTML
} )


#' inverts the time in the cellexalLinear object.
#' 
#' Occational the algorithm fails and inverts the linear order.
#' This function is used to easily invert a (inverted) linearSelection that 
#' runs from end to start instead the otherway around.
#' @name invert
#' @docType methods
#' @description reverse the time orientaion
#' @param x the object
#' @title reverse the time order for a cellexalLinear object
#' @export 
#if ( ! isGeneric('invert') ){
setGeneric('invert', ## Name
function ( x ) { 
	standardGeneric('invert')
}
)
#}



#' @rdname invert
setMethod('invert', signature = c ('cellexalLinear'),
	definition = function ( x ) {
	oldT = x
	x@dat$time = (x@dat$time -max(x@dat$time))*-1
	old = names(table(x@dat$col))
	new = rev(old)
	newV = as.vector(x@dat$col)
	for (i in 1:length(new)){
		newV[which(x@dat$col == old[i])] = new[i]
	}
	x@dat$col = factor( newV, levels=old )
	x@dat$order = x@dat$order[nrow(x@dat):1]
	x@dat = x@dat[order( x@dat$time),]
	invisible(x)
} )


#' Plot the linear data with the correct colors.
#' Used to debug the linear selections.
#' 
#' @name plotTime
#' @docType methods
#' @description 3D plot the cellexalLinear object
#' @param x the object
#' @title plot the drc with pseudo time colors
#' @export 
#if ( ! isGeneric('plotTime') ){
setGeneric('plotTime', ## Name
function ( x ) { 
	standardGeneric('plotTime')
}
)
#}



#' @rdname plotTime
setMethod('plotTime', signature = c ('cellexalLinear'),
definition = function ( x ) {
	#if ( var(x@dat[,'c'] )== 0 ) {
	plot( x@dat$a, x@dat$b, col=as.vector(x@dat$col))
	#lines( x@dat$x, x@dat$y, col='green', lwd=4)
	#}
	#else {
	#	rgl::plot3d( x@dat[,c('a','b','c')], col= as.vector(x@dat[,'col']) )
	#	rgl::points3d( x@dat[,c('x','y','z')], col='green')
	#}
} )


#' @name plotDataOnTime_rmd
#' @docType methods
#' @description plot linear data on the speudo time in a 2d figure
#' @param x the object
#' @param dat the linear data to plot
#' @param color the color(s) the data should be plotted in
#' @param ofile the figure file (png)
#' @param cellexalObj the cellexal object to adjust the path for the markdown file.
#' @title create the Rmd figure strings for the plotTime files
#' @export 
#if ( ! isGeneric('plotDataOnTime_rmd') ){
setGeneric('plotDataOnTime_rmd', ## Name
function ( x, dat, color=NULL, ofile, cellexalObj  ) { 
	standardGeneric('plotDataOnTime_rmd')
}
)
#}

#' @rdname plotDataOnTime_rmd
setMethod('plotDataOnTime_rmd', signature = c ('cellexalLinear'),
definition = function ( x, dat, color=NULL, ofile, cellexalObj ) {
	plotDataOnTime( x, dat, color=NULL, ofile )
	return(paste("![](", correctPath(ofile, cellexalObj),")" ))
})


#' Plot a list of data values on the linearSelection the vectors in the list must have cell names
#' @name plotDataOnTime
#' @docType methods
#' @description plot linear data on the linear selection in a 2d figure
#' @param x the cellexalLinear object or a data.frame (use the cellexalLinear object)
#' @param dat the linear data to plot
#' @param color the color(s) the data should be plotted in
#' @param ofile the figure file (png)
#' @param smooth add a smoothed line to the graph (default TRUE)
#' @title plot data on the cellexalLinear time information as XY plots
#' @export 
#if ( ! isGeneric('plotDataOnTime') ){
setGeneric('plotDataOnTime', ## Name
function ( x, dat, color=NULL, ofile, smooth=TRUE ) { 
	standardGeneric('plotDataOnTime')
}
)
#}

#' @rdname plotDataOnTime
setMethod('plotDataOnTime', signature = c ('cellexalLinear', 'list'),
definition = function ( x, dat, color=NULL, ofile, smooth=TRUE ) {
	toPlot = data.frame(x@dat[,c('time', 'col')])
	plotDataOnTime( x= toPlot, dat=dat, color=color, ofile=ofile, smooth=smooth)
})


#' @rdname plotDataOnTime
setMethod('plotDataOnTime', signature = c ('data.frame', 'list'),
	definition = function ( x, dat, color=NULL, ofile, smooth=TRUE ) {
	if ( is.null(color) ) {
		color = grDevices::rainbow( length(dat))
	}
	if ( length(color) != length(dat) ){
		message("I got less or more colors than data column - change to rainbow")
		color = grDevices::rainbow( length(dat))
	}
	toPlot = x

	for ( n in names(dat) ){
		toPlot[,n] = NA
		if ( is.null(names(dat[[n]]))){
			stop( "plotDataOnTime dat needs cell names!")
		}
		m = match( names(dat[[n]]), rownames(toPlot))
		if ( length(which(is.na(m))) > 0 ){
			print("missing values in the data!")
			if(interactive()) { browser() }
		}
		toPlot[m,n] =dat[[n]]
		## now the first smoothing run
		span = span= 40 / length(m)
		if ( span == Inf){
			span = 0.02
		}
		pred1 = tryCatch({
		 stats::loess(  toPlot[m,n] ~ toPlot[m,'time'], span=span )},
		 error=function(err){ stop(paste("loess failed with span", span, " - no values selected (n=",length(m),")?!")) })	

		toPlot[m,n] = stats::predict(pred1)
		## if you want to take a closer look:
		#plot( toPlot[m,'time'],dat[[n]])
		#points(toPlot[m,'time'], toPlot[m,n], col='blue' )F

	}
	
	mi = min(unlist(dat))
	ma = max(unlist(dat))
	xstarts = as.vector(toPlot$time[match( unique(toPlot$col), toPlot$col)])
	xstarts[1] = -Inf
	col = as.vector(toPlot$col[match( unique(toPlot$col), toPlot$col)])
	xends =as.vector(c(xstarts[-1],toPlot$time[nrow(toPlot)] )) 
	xends[length(xends)] = Inf
	rects = data.frame( xstarts, xends, col)
	rects$col = as.vector(rects$col)
	rects$col = factor( rects$col, levels=rects$col)
	#print ( "plotDataOnTime create plot" )
	grDevices::png( filename=ofile, width=1000, height = 1000)
	wes = function(n) {wesanderson::wes_palette("Zissou1", n,type = "continuous")[1:n] }
	pl = ggplot2::ggplot(toPlot, ggplot2::aes( xmin = min(time), xmax= max(time), ymin= mi, ymax=ma) )
	pl = pl + ggplot2::theme_classic() +
	# ggplot2::geom_rect(data=rects,mapping = ggplot2::aes(
	#	xmin = xstarts, 
	#	xmax = xends, 
	#	#ymin = mi, 
	#	#ymax = mi+ (ma -mi)/10 
	#	ymin = -Inf,
	#	ymax = Inf
	#	),
  	#	fill= wesanderson::wes_palette("Zissou1",10, type = "continuous")[1:10],
	#	alpha = .2) +
  ggplot2::scale_fill_manual( 
  	palette = wes,
  	values = wesanderson::wes_palette("Zissou1", 10,type = "continuous")[1:10],
  	aesthetics = c("colour", "fill")
  ) +  
  ggplot2::guides(fill="none")


	for ( a in 1:length(dat) ){
		n = names(dat)[a]
		pl = pl + ggplot2::geom_smooth(data=toPlot, 
		  	mapping=ggplot2::aes_string(x='time', y= n, alpha=.6 ), color=color[a], 
		  	method=loess, fill=color[a], alpha=.2, na.rm = TRUE)	

		circleF = paste(sep=".", ofile,a,'svg' )	
		fileConn<-file( circleF )
		writeLines(c(
			'<svg viewBox="0 0 100 100" xmlns="http://www.w3.org/2000/svg">', 
			paste( sep="",'  <g color="',color[a],'">'),
			paste(sep="",'    <circle cx="50" cy="50" r="50" fill="',color[a],'"/>'),
			'  </g>',
			'</svg>'
		), fileConn)
		close(fileConn)
	}
	#pl = pl + ggplot2::theme(panel.background = ggplot2::element_blank())
	pl = pl + ggplot2::ggtitle('Gene sets expression changes over the selected linear selection')
	pl = pl + ggplot2::ylab( "Smoothed mean expression of gene sets" )
	pl = pl + ggplot2::xlab( "pseudotime" )
	## get the global min and max.
	dat= data.frame(lapply ( ggplot2::ggplot_build(pl)$data , function( dat ){ c( min(dat$ymin), max(dat$ymax) )} ) )

	mi = min(dat[1,])
	ma = max(dat[2,])
	pl = pl + 
 		ggplot2::geom_rect(data=rects,mapping = ggplot2::aes(
		xmin = xstarts, 
		xmax = xends, 
		#ymin = mi, 
		#ymax = mi+ (ma -mi)/10 
		ymin = -Inf,
		ymax = mi
		),
  		fill= wesanderson::wes_palette("Zissou1",10, type = "continuous")[1:10],
		alpha = 1) + 
	  	ggplot2::geom_rect(data=rects,mapping = ggplot2::aes(
		xmin = xstarts, 
		xmax = xends, 
		#ymin = mi, 
		#ymax = mi+ (ma -mi)/10 
		ymin = ma,
		ymax = Inf
		),
  		fill= wesanderson::wes_palette("Zissou1",10, type = "continuous")[1:10],
		alpha = 1) 

	print(pl) # write the plot data
	grDevices::dev.off()
	#print ("plotDataOnTime finished")

} )


#' @name plotTimeHeatmap
#' @docType methods
#' @description create a really simple heatmap of the zscored data
#' @param x the zscored data object
#' @param ofile the figure file (a '.png' will be added)
#' @param color the color of the border around the heatmap
#' @param circleF a file for a small colored circle as svg file (needed for the reports)
#' @title description of function plotTimeHeatmap
#' @returns create a heatmap figure file for a cellexalLinear object
#' @export
#if ( ! isGeneric('plotTimeHeatmap') ){
setGeneric('plotTimeHeatmap', ## Name
function ( x, ofile, color=NULL, circleF=NULL ) { 
	standardGeneric('plotTimeHeatmap')
}
)
#}

#' @rdname plotTimeHeatmap
setMethod('plotTimeHeatmap', signature = c ('matrix'),
definition = function ( x, ofile, color=NULL, circleF=NULL) {
	of2 = paste( sep=".", ofile, 'png')
	h =  nrow(x) * 2
	if (h < 200 ) { h=200 }
	#print ( paste( "plotTime heatmap has set the height of the heatmap to", h) )
	#ggplot( genes2plot, aes( x=id,y=variable, fill=value))+geom_tile() + 
	#scale_fill_gradient(high='yellow', low='blue' ) +
	# xlim(0, max(toPlot$id))

	grDevices::png( filename=of2, width=1000, height = h )
	try ( {
		graphics::image( t(x), col=gplots::bluered(40) )
		if( ! is.null(color))  {
			box("outer", col=color, lwd = 10)
		}
	})
	grDevices::dev.off()

	## I got the user request to recall the color in the grouping.
	## As this color can be user defined I need to store this info here!
	## easiest might be a 
	if ( ! is.null(circleF)){
		fileConn<-file( circleF )
		writeLines(c(
			'<svg viewBox="0 0 100 100" xmlns="http://www.w3.org/2000/svg">', 
			paste( sep="",'  <g color="',color,'">'),
			paste(sep="",'    <circle cx="50" cy="50" r="50" fill="',color,'"/>'),
			'  </g>',
			'</svg>'
		), fileConn)
		close(fileConn)
	}
	# markdown : <img src="circleF" width="200">

	of2
})


#' convert the pngs from plotTimeHeatmap to Rmd figures.
#' @name plotHeatmap_rmd
#' @docType methods
#' @description plot linear data on the time in a 2d figure
#' @param x the matrix to plot (genes rows cells cols)
#' @param ofile the figure file (png)
#' @param cellexalObj the cellexal object to adjust the path for the markdown file.
#' @param color the color(s) the data should be plotted in
#' @title clip the figure path for using in the log system
#' @export 
#if ( ! isGeneric('plotHeatmap_rmd') ){
setGeneric('plotHeatmap_rmd', ## Name
function ( x, ofile, cellexalObj, color=NULL ) { 
	standardGeneric('plotHeatmap_rmd')
}
)
#}

#' @rdname plotHeatmap_rmd
setMethod('plotHeatmap_rmd', signature = c ('matrix'),
definition = function ( x, ofile, cellexalObj, color=NULL ) {
	ofile = plotTimeHeatmap( x, ofile = ofile, color = color )
	return(paste("![](", R.utils::getRelativePath(ofile, cellexalObj@outpath),")" ))
})


#' @name pseudoBulk
#' @docType methods
#' @description mean collapses the data to contain an even amount of single cells per pseudo bulk sample
#' @param object the cellexalGrouping
#' @param cellexalObj the corresponding cellexal Object
#' @param minSize the minimum size of a group (default 10 (cells))
#' @param minBulks the minimal number of pseudo bulk samples (default 10)
#' @param maxBulks the maximum number of pseudo bulk samples (default 500)
#' @title show for a cellexalGrouping object
#' @returns a cellexalvrR object where each cell represents a pseudo sample.
#' @export


setGeneric('pseudoBulk', ## Name
	function ( object, cellexalObj, minSize=10, minBulks = 10, maxBulks = 500 ) { 
		standardGeneric('pseudoBulk')
	}
)

setMethod('pseudoBulk', signature = c ('cellexalLinear'),
	definition = function ( object, cellexalObj, minSize=10, minBulks = 10, maxBulks = 500 ) {

		tab = table( object@dat$col)
		min_cells = min(tab)
		toCells = minSize
		if ( floor(min_cells/minSize) < minBulks ){
			warning( "one group has very view cells - created less pseudo samples that wanted")
			toCells = minSize
			if ( floor(min_cells/toCells) < 2 ){
				## that is really really too little cells here!
				toCells = ceiling(min_cells/2)
			}
		}else if ( floor(max(tab)/minSize) > maxBulks ){
			toCells = ceiling(max(tab)/maxBulks)
			if ( floor(min_cells/toCells) < minBulks ){
				toCells = floor(max(tab)/minBulks)
				warning( paste("using more than",maxBulks,"max pseudo samples in a group"  ) )
			}
		}

		toIDs = function(n) {
			toGroups = floor(n / toCells)
			if ( toGroups  == 0 ){
				toGroups = 2
			}
			if ( is.na(toGroups)){
				browser(text="toGroups is not defined!")
			}
			tryCatch( {
				ids = rep( 1:toGroups, floor(n/toGroups))
				},
				error = function(err){ print(err); browser( text="in the try catch construct an error has occured")})
			ids = rep( 1:toGroups, floor(n/toGroups))
			ids = c( ids, sample( 1:toGroups, n%%toGroups))
			ids = sort(ids)
			ids
		}

		cellexalObj = reorderSamples ( cellexalObj, object@gname )
		object = groupingInfo(cellexalObj, object@gname)@linarObj

		ids =  toIDs(tab[1])
		for (n in tab[-1]) {
			ids = c( ids, toIDs(n) + max(ids))
		}

		# now I need to construct the pseudoBulk object...
		PseudoBulk = reduceTo( cellexalObj, what='col', to= rownames(object@dat)[match(unique(ids), ids)])
		#browser()
		PseudoBulk@data = Matrix(FastWilcoxTest::collapse( cellexalObj@data[,rownames(object@dat)], ids, type=2 ), sparse=TRUE) ## mean expression

		rownames(PseudoBulk@data) = rownames(cellexalObj@data)
		colnames(PseudoBulk@data) = rownames(object@dat)[match(unique(ids), ids)]
		invisible ( PseudoBulk )
		
	}
)



#' describesTheseCells - check if this object describes these cells
#'
#' @name describesTheseCells
#' @docType methods
#' @description describesTheseCells - check if this object describes these cells
#' @param x the object
#' @param cells the cell names to subset the object to
#' @title a simple check that does what ist is called for
#' @export 
#' @rdname describesTheseCells
setGeneric('describesTheseCells', ## Name
	function ( x, cells ) { 
		standardGeneric('describesTheseCells')
	}
)

setMethod('describesTheseCells', signature = c ('cellexalLinear', 'character'),
definition = function ( x, cells ) {
	a = unique( c(rownames(x@dat), cells ))
	length(a) == nrow(x@dat) && length(a) == length(cells)	
}
)

#' get the (cell) names from this time
#'
#' @name names
#' @docType names
#' @description subset a cellexalLinear object
#' @param x the object
#' @title cellnames of a cellexalLinear object
#' @export 
setMethod('names', signature = c ('cellexalLinear'),
definition = function ( x) {
	return( rownames(x@dat) )
} )


#' Subset a cellexalLinear object based on a list of cells.
#'
#' @name subset
#' @docType methods
#' @description subset a cellexalLinear object
#' @param x the object
#' @param cells the cell names to subset the object to
#' @title subset a cellexalLinear object
#' @export 
setMethod('subset', signature = c ('cellexalLinear'),
definition = function ( x, cells) {
	m = match( cells, rownames(x@dat) )
	m = m[which(!is.na(m))]
	x@dat = x@dat[m,]
	## the merged data - if existing - becomes invalid
	if ( ! is.null( x@geneClusters[['collapsedExp']])) {
		x@geneClusters[['collapsedExp']] = NULL
	}
	x@dat = x@dat[order(x@dat$time),]
	x@id = digest::digest( x@dat, algo="md5")
	invisible(x)
} )


#' To run this method efficiently it is recommended to first subset the drc the selection is based on.
#' Next it is recommended to add this drc subset to the cellexal object and use the subset function to also subset the linearSelection.
#' This way one can assure that the timepoints can be compared across the analyses.
#' This function will run the statistics and create a report for the new subset.
#' 
#' This function is highly experimental and not meant to be used from within VR!
#' 
#' @name linearAnalysisSubset
#' @docType methods
#' @description create a time report for a subset of cells only
#' @param x the object
#' @param cellexalObj the cellexal object to process
#' @param deg.genes summary over these genes of the cellexalObj@usedObj$deg.genes populated during the createStats() run
#' @title old method combining createStats and createReport functions
#' @export 
#if ( ! isGeneric('linearAnalysisSubset') ){
setGeneric('linearAnalysisSubset', ## Name
function ( x, cellexalObj, deg.genes=NULL ) { 
	standardGeneric('linearAnalysisSubset')
}
)
#}


#' @rdname linearAnalysisSubset
setMethod('linearAnalysisSubset', signature = c ('cellexalLinear'),
definition = function ( x, cellexalObj, deg.genes=NULL ) {

	stop("This function is likely broken - debug it if you really need it")

	loc = reduceTo( cellexalObj,what='col', to= rownames(x@dat) )
	loc = createStats( x, loc )

	if ( is.null(deg.genes) ){
		deg.genes = loc@usedObj$deg.genes
	}

	cellexalObj@usedObj$deg.genes = deg.genes
	
	cellexalObj = addSelection(cellexalObj, groupingInfo( loc, x@gname))

	if ( is.null(cellexalObj@usedObj$sigGeneLists$lin) ) {
		cellexalObj@usedObj$sigGeneLists$lin = list()
	}
	message("This next step should not be necessary!")
	browser()
	cellexalObj@usedObj$sigGeneLists$lin[[x@gname]] = loc@usedObj$sigGeneLists$lin[[x@gname]]
	
	loc = reduceTo(cellexalObj, what='row', to=deg.genes)
	loc = createStats( x, loc )
	loc = createReport(x, loc, groupingInfo( cellexalObj, x@gname ) )

	x = ret$linearSelection
	#cellexalObj = ret$cellexalObj
	cellexalObj = addSelection( x, cellexalObj, x@parentSelection)

	invisible( cellexalObj )

} )
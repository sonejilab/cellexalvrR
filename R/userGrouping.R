#' VR function that reads the VR grouping information and stores it in the R object.
#' Each selection is only added once into the cellexalvrR object.
#' @name userGrouping
#' @docType methods
#' @description  Reads a VR cell selection file and creates a user.grouping column with the information
#' @description  storing the user defined grouping for later use
#' @param cellexalObj, cellexalvr object
#' @param cellidfile file containing cell IDs
#' @title add a CellexalVR created selection to the cellexalvrR object
#' @export 
#if ( ! isGeneric('userGrouping') ){
setGeneric('userGrouping', ## Name
	function (cellexalObj, cellidfile) { 
		standardGeneric('userGrouping') 
	}
)
#}


#' @rdname userGrouping
setMethod('userGrouping', signature = c ('cellexalvrR'),
	definition = function (cellexalObj, cellidfile) {
	
	cellexalObj <- renew(cellexalObj) #function definition in file 'renew.R'
	save = FALSE
	
    ## try to cut short - is the grouping already known?

    info = try({suppressMessages(suppressWarnings(groupingInfo( cellexalObj, cellidfile)))}, silent=TRUE)
    if ( class(info) == 'cellexalGrouping'){
    	# the old way breaks if there is a path in this slot.
    	# So let's try to use the group name instead?!
    	#cellexalObj@usedObj$lastGroup = cellidfile
		cellexalObj@usedObj$lastGroup = info@gname
    	return ( cellexalObj )
    }
	#init local vars
	id= (ncol(cellexalObj@userGroups) /2) + 1
	gname = paste( "User.group", id, sep="." ) #the VR program dependeds on it
	gOrderName = paste( gname, 'order', sep=" ")

	n = vector( 'numeric', ncol(cellexalObj@data))
	possibleNames = unique(unlist(lapply( colnames(cellexalObj@meta.cell), 
			function(x) { str_split(x, "@")} )))

	if ( file.exists(cellidfile) ){ ## file from the VR env
		## add this group into the object
		cellid <- utils::read.delim(cellidfile,header=F)
		if ( cellid[1,1] == 'CellID'){ ## reading a file from the session path
			cellid = cellid[-1,]
		}
		sessionStoreFile <- function() {
			if ( ! is.null(cellexalObj@usedObj$sessionPath) ){## active session - we need to copy the cellidfile to this path
				if ( ! file.exists(cellexalObj@usedObj$sessionPath)){
					dir.create( cellexalObj@usedObj$sessionPath, recursive=T)
				}
				if ( ! file.exists( file.path( cellexalObj@usedObj$sessionPath,basename(dirname(cellidfile)))) ){
					dir.create( file.path( cellexalObj@usedObj$sessionPath,basename(dirname(cellidfile)))) 
				}
				# change in CellexalVR - the tool now stores the selections in folders - argh
				selectionF = file.path(basename(dirname(cellidfile)), basename(cellidfile))

				if ( file.exists( cellidfile ) && 
					! file.exists( file.path(cellexalObj@usedObj$sessionPath, selectionF )) ) {
					file.copy(cellidfile, file.path(cellexalObj@usedObj$sessionPath, selectionF ))
					#cat( gname ,file=file.path( dirname(cellidfile)), paste( basename(cellidfile), 'group', 'txt', sep=".") ),append=TRUE)
				}
			}
		}
		#browser()
		#find overlap with own data
		m = match(cellid[,1], colnames(cellexalObj@data))
		if ( length(which(is.na(m))) > 0 ){
			stop( paste (
				"The grouping file",cellidfile," has cells that are not part of the object:", 
				paste(as.vector(cellid[which(is.na(m)),1]), collapse=", " )
					))

		}

		m = match(colnames(cellexalObj@data), cellid[,1])
		n = rep( NA, ncol(cellexalObj@data))
		OK = m[ which(! is.na(m)) ]
		if ( length(which(is.na(as.numeric(cellid[OK,4]) ))) == 0){
			n[ which(! is.na(m)) ] = cellid[m[ which(! is.na(m)) ],4] +1
		}
		else {
			n[ which(! is.na(m)) ] = cellid[m[ which(! is.na(m)) ],4]
		}
		
		names(n) = colnames(cellexalObj@data)
		order = n
		order[match(cellid[,1], colnames(cellexalObj@data))] = 1:nrow(cellid)
		new =(cbind(n, order))
		colnames(new) = c( gname, gOrderName)

		if ( ncol(cellexalObj@data) != nrow(cellexalObj@userGroups)){
			## Just create a new one...
			id = 1
			gname = paste( "User.group", id, sep="." )
			gOrderName = paste( gname, 'order', sep=" ")
			new = data.frame( new )
			colnames(new) = c( gname, gOrderName)

			cellexalObj@userGroups = new 
			sessionStoreFile() ## local function
		}else {
			# did we already read this file?
			t <- apply( cellexalObj@userGroups, 2, function ( x ) { ok = 
				all.equal(as.numeric(as.vector(x)),as.numeric(as.vector(n))); if ( ok == T ) {TRUE } else { FALSE } } )
			d <- apply( cellexalObj@userGroups, 2, function ( x ) { ok = 
				all.equal(as.numeric(as.vector(x)),as.numeric(as.vector(order))); if ( ok == T ) {TRUE } else { FALSE } } )
			ok <- which( t == T )
			if ( length(ok) == 1 & length( which(d == T)) == 1) {
				## OK use the old one
				gname = names(ok)
				id = ceiling(as.vector(ok) / 2)
				sessionStoreFile()
			}else {
				cellexalObj@userGroups = cbind(cellexalObj@userGroups, new)
				sessionStoreFile() ## local function
			}
		}

		colorIDs = names(table(cellid[,4]))
		colVR = rep( NA, max(colorIDs))
		colVR[colorIDs] = cellid[match( colorIDs,cellid[,4]),2]

		#colVR = colR =  cellid[match( colorIDs,cellid[,4]),2]
		#colVR = c()
		#for ( i in 1:length(colR)) {
		#	colVR[colorIDs[i]] = as.character(colR[i])
		#}
		## look into the drs - do all cells come from one single drc?
		drc = unique(as.vector(cellid[,3]))
		realDRC = drc
		for ( name in realDRC ){## could already be more
			m = match(cellid[,1], rownames( cellexalObj@drc[[name]]) )
			if ( length(which(is.na(m))) > 0 ){## lets hope they have been selected from a different drc ;-)
				badIDs = which(is.na(m))
				badCells = cellid[badIDs,1]
				matches = unlist(lapply( cellexalObj@drc, function(x) { length(which(!is.na(match(badCells, rownames(x) ))))} ) )
				goodDRC = names(cellexalObj@drc)[which(matches == max(matches))[1]]
				drc = c(drc, goodDRC)
			}
		}

		drc = unique( drc )
        if ( !is.factor(cellexalObj@userGroups[,gname]) ){
        	cellexalObj@userGroups[,gname] = factor( cellexalObj@userGroups[,gname] )
        }

        if ( class(colVR)[1] != "character"){
        	browser()
        }

		ginfo = new( 'cellexalGrouping',
			gname = gname,
			selectionFile= cellidfile ,
			grouping = cellexalObj@userGroups[,gname],
			order = as.integer(order),
			'drc' = drc,
			col = colVR
		)
		names(ginfo@grouping) = rownames(cellexalObj@userGroups)
		
		if ( ! is.na(match(paste(cellexalObj@usedObj$lastGroup, 'order', sep=" "), colnames(cellexalObj@data))) ){
			ginfo@order = cellexalObj@userGroups[,paste(gname, 'order', sep=" ")]
		}

		cellexalObj@groupSelectedFrom[[gname]] = ginfo
		cellexalObj@colors[[gname]] = colVR

		cellexalObj = checkGrouping( cellexalObj, gname )

		savePart ( cellexalObj, 'groupSelectedFrom') #function definition in file 'integrateParts.R'
		savePart ( cellexalObj, 'colors') #function definition in file 'integrateParts.R'
		savePart ( cellexalObj, 'userGroups') #function definition in file 'integrateParts.R'
		
	}
	else if ( is.na(match(cellidfile, colnames(cellexalObj@userGroups))) ==F ) {
		## the grouping is known
		gname = cellidfile
	}
	else if ( !is.na( match(cellidfile, c( colnames(cellexalObj@meta.cell),possibleNames) ) ) ){
		## merge the shitty split of info back into a usable format!
		toMerge = grep( cellidfile, colnames(cellexalObj@meta.cell) )
		grouping = NULL
		order = NULL
		if (length(grep (cellidfile, possibleNames) ) == 1 &&  length(toMerge) == 1){
			grouping = cellexalObj@meta.cell[, toMerge] +1 
			names(grouping) = colnames(cellexalObj@data)
			order = 1:ncol(cellexalObj@data)
		}
		else if ( length(grep (cellidfile, possibleNames) ) == 1 &&  length(toMerge) > 1){
			entries = unique(unlist(lapply( colnames(cellexalObj@meta.cell)[toMerge], 
			function(x) { str_split(x, "@")[[1]][2]} )))
			grouping = rep( NA, ncol(cellexalObj@data))
			names(grouping) = colnames(cellexalObj@data)
			order = 1:ncol(cellexalObj@data)
			for ( id in 1:length(toMerge) ){
				entryname = paste( sep="@",cellidfile,toMerge[id])
				if ( is.na(match ( entryname, colnames(cellexalObj@meta.cell) )) ){
					entryname = paste( sep="@",toMerge[id],cellidfile)
				}
				grouping[ which(cellexalObj@meta.cell[,toMerge[id]] == 1)] = entries[id]
			}
		}else {
			stop(paste( "sorry, but you can not merge these groups:", paste(collapse=", ",colnames(cellexalObj@meta.cell)[toMerge] )) )
		}
		tmp = data.frame( A = factor(grouping), B= order  )
		colnames( tmp ) = c(cellidfile,  paste(cellidfile, 'order', sep=" ") )
		rownames(tmp) = colnames(cellexalObj@data)
		if ( ncol(cellexalObj@userGroups) == 0){
			cellexalObj@userGroups = tmp
		}else {
			cellexalObj@userGroups = cbind( cellexalObj@userGroups, tmp )
		}
		if ( !is.factor(cellexalObj@userGroups[,cellidfile]) ){
        	cellexalObj@userGroups[,cellidfile] = factor( cellexalObj@userGroups[,cellidfile] )
        }
		ginfo = new( 'cellexalGrouping',
			gname = cellidfile,
			selectionFile= "InternalGrouping",
			grouping = cellexalObj@userGroups[,cellidfile] ,
			order = cellexalObj@userGroups[,paste(cellidfile, 'order', sep=" ")],
			'drc' = names(cellexalObj@drc)[1],
			col = grDevices::rainbow( length(unique( cellexalObj@userGroups[,cellidfile])) )
		)
		names(ginfo@grouping) = rownames(cellexalObj@userGroups)

		cellexalObj@groupSelectedFrom[[cellidfile]] = ginfo

		cellexalObj@usedObj$SelectionFiles[[cellidfile]] = ""
		gname = cellidfile
		names(ginfo@grouping) = rownames(cellexalObj@userGroups)
	}
	else if( ! is.null(cellexalObj@usedObj$samples) && 
		!is.na( match(cellidfile,colnames(cellexalObj@usedObj$samples) ) ) ){
		## use one of the orig cell info
		df = data.frame( cellexalObj@usedObj$samples[, cellidfile])
		colnames(df) = cellidfile
		cellexalObj@usedObj$samples[, paste(cellidfile, 'order') ] = 1:nrow(df)
		if ( ncol(cellexalObj@userGroups) == 0 ){
			cellexalObj@userGroups = df
		}else {
			cellexalObj@userGroups = cbind( cellexalObj@userGroups, df )
		}
		ginfo = groupingInfo( cellexalObj, cellidfile )
		if ( class(ginfo)[1] == 'cellexalvrR'){
			cellexalObj = ginfo
			ginfo = groupingInfo( cellexalObj, cellidfile )
		}
		cellexalObj@groupSelectedFrom[[cellidfile]] = ginfo
	}
	else {
		stop( paste("Cannot process the request for grouping",cellidfile,"\nIt is neither a file nor a known user grouping name\n" ) )
	}
	## store the grouing name
	cellexalObj@usedObj$lastGroup = gname
	if ( is.null(cellexalObj@usedObj$SelectionFiles)) {
		cellexalObj@usedObj$SelectionFiles = list()
	}
	if ( file.exists(cellidfile) ){
		cellexalObj@usedObj$SelectionFiles[[gname]] = cellidfile
	}else{
		cellexalObj@usedObj$SelectionFiles[[gname]] = paste(sep="","Selection",id,".txt" )
	}
	savePart ( cellexalObj, 'usedObj') #function definition in file 'integrateParts.R'
	cellexalObj
} )

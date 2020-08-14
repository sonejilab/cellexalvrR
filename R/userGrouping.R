#' VR function that ready the VR grouping information and stores it in the R object.
#' @name userGrouping
#' @aliases userGrouping,cellexalvrR-method
#' @rdname userGrouping-methods
#' @docType methods
#' @description  Reads a VR cell selection file and creates a user.grouping column with the information
#' @description  storing the user defined grouping for later use
#' @param cellexalObj, cellexalvr object
#' @param cellidfile file containing cell IDs
#' @param cellidfile  TEXT MISSING
#' @keywords userGrouping
#' @title description of function userGrouping
#' @export userGrouping
if ( ! isGeneric('userGrouping') ){setGeneric('userGrouping', ## Name
	function (cellexalObj, cellidfile) { 
		standardGeneric('userGrouping') 
	}
) }

setMethod('userGrouping', signature = c ('cellexalvrR'),
	definition = function (cellexalObj, cellidfile) {
	
	cellexalObj <- renew(cellexalObj) #function definition in file 'renew.R'
	save = FALSE
	#init local vars
	id= (ncol(cellexalObj@userGroups) /2) + 1
	gname = paste( "User.group", id, sep="." ) #the VR program dependeds on it
	gOrderName = paste( gname, 'order', sep=" ")

	n = vector( 'numeric', ncol(cellexalObj@data))

	if ( file.exists(cellidfile)){ ## file from the VR env
		## add this group into the object
		cellid <- utils::read.delim(cellidfile,header=F)
		if ( cellid[1,1] == 'CellID'){ ## reading a file from the session path
			cellid = cellid[-1,]
		}
		sessionStoreFile <- function() {
			if ( ! is.null(cellexalObj@usedObj$sessionPath) ){## active session - we need to copy the cellidfile to this path
				if ( file.exists( cellidfile )){
					file.copy(cellidfile, file.path(cellexalObj@usedObj$sessionPath,""))
					cat( gname ,file=file.path(cellexalObj@usedObj$sessionPath, paste( basename(cellidfile), 'group', 'txt', sep=".") ),append=TRUE)
				}
			}
		}
	#	browser()
		#find overlap with own data
		m = match(cellid[,1], colnames(cellexalObj@data))
		if ( length(which(is.na(m))) > 0 ){
			stop( paste (
				"The grouping file has cells that are not defined in the object:", 
				paste(as.vector(cellid[which(is.na(m)),1]), collapse=", " )
					))

		}
		m = match(colnames(cellexalObj@data), cellid[,1])
		n = rep( NA, ncol(cellexalObj@data))
		n[ which(! is.na(m)) ] = cellid[m[ which(! is.na(m)) ],4] +1
		
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

		ginfo = list(
			gname = gname,
			selectionFile= basename( cellidfile ),
			grouping = cellexalObj@userGroups[,gname] ,
			order = 1:ncol(cellexalObj@data),
			'drc' = unique(as.vector(cellid[,3])),
			col = unique(as.vector(unique(cellid[,2])))
		)
		if ( ! is.na(match(paste(cellexalObj@usedObj$lastGroup, 'order', sep=" "), colnames(cellexalObj@data))) ){
			ginfo[['order']] = cellexalObj@userGroups[,paste(gname, 'order', sep=" ")]
		}
		cellexalObj@groupSelectedFrom[[gname]] = ginfo
		cellexalObj@colors[[gname]] = unique(as.vector(unique(cellid[,2])))
		savePart ( cellexalObj, 'groupSelectedFrom') #function definition in file 'integrateParts.R'
		savePart ( cellexalObj, 'colors') #function definition in file 'integrateParts.R'
		savePart ( cellexalObj, 'userGroups') #function definition in file 'integrateParts.R'
		
	}
	else if ( is.na(match(cellidfile, colnames(cellexalObj@userGroups))) ==F ) {
		## the grouping is known
		gname = cellidfile
	}else {
		stop( paste("Cannot process the request for grouping",cellidfile,"\nIt is neither a file nor a known user grouping name\n" ) )
	}
	## store the grouing name
	cellexalObj@usedObj$lastGroup = gname
	if ( is.null(cellexalObj@usedObj$SelectionFiles)) {
		cellexalObj@usedObj$SelectionFiles = list()
	}
	if ( file.exists(cellidfile) ){
		cellexalObj@usedObj$SelectionFiles[[gname]] = cellidfile
	}
	savePart ( cellexalObj, 'usedObj') #function definition in file 'integrateParts.R'
	cellexalObj
} )

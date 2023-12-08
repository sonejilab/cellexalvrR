#' add a selection to the cellexalvrR object
#' This function is not necessary for the VR integration.
#' 
#' @name addSelection
#' @docType methods
#' @description  returns the information stored for the last grouping read
#' @param cellexalObj, cellexalvr object
#' @param sel the cellexal selection object 
#' @title integrate a selection
#' @return an unpdated cellexalvrR object
#' @export 
#if ( ! isGeneric('groupingInfo') ){
setGeneric('addSelection', ## Name
	function ( cellexalObj, sel ) { 
		standardGeneric('addSelection') 
	}
)
#}

#' @rdname addSelection
setMethod('addSelection', signature = c ('cellexalvrR', 'cellexalLinear'), 
	definition = function ( cellexalObj, sel )  {

        sel = checkTime(sel) # might need a new ID?
		overlapp = lapply( cellexalObj@usedObj$linearSelections, function(x) { x@id == sel@id } )
        same = names(overlapp)[unlist(overlapp)]
        
        if ( length(same) >1 ){
            len = length(same)
            for ( name in same) {
                if (name == "lastEntry"){
                    len = len -1
                }
            }

            if (len >1){
                #print("Library Error - I have a duplicate linear object in my internal list - fix me!")
                print("Multiple cellexalLinear objects describing the same cells - does that make sense here?")
            }
        }
        if ( length(same) == 1 ){
            #print(paste( "This time object already exixts - renamed old time object to",sel@gname ))
            cellexalObj@usedObj$linearSelections[[same]] = sel
            cellexalObj@groupSelectedFrom[[sel@parentSelection]]@linarObj = sel
            cellexalObj@usedObj$linearSelections[['lastEntry']] = sel    
            return(invisible(cellexalObj))
        }
        # so now I know it is new and should be added
        # do we have it's parent selection?
        parentSelection = NULL
        if ( is.null( sel@parentSelection ) ){
            ## create it from scratch
            #warning("I need to create a new parent selection from scratch!")
        	cellexalObj = createGrouping ( cellexalObj, cells=names(sel), 
                groups = rep(0, ncol(sel@dat)), col= "#34b1eb", drc = sel@drc  )
            parentSelection = groupingInfo( cellexalObj )
        }else {
            parentSelection = groupingInfo(cellexalObj, sel@parentSelection)
            if ( ! describesTheseCells(parentSelection, names(sel) )){
                ## not surprising!
                parentSelection = subset( parentSelection, names(sel) )
            }
        }

        # now finally register the new time in the object!
        if ( length(grep ("bseudoBulk", sel@gname)) == 0 ){
            cellexalObj@usedObj$SelectionFiles[[parentSelection@gname]] = parentSelection@selectionFile
            cellexalObj@usedObj$SelectionFiles[[sel@gname]] = parentSelection@selectionFile
        }
        
        cellexalObj@usedObj$linearSelections[[sel@gname]] = sel
        cellexalObj@groupSelectedFrom[[sel@parentSelection]]@linarObj = sel

        ## is the time represented in the userGroups table?
        if ( is.na( match( sel@gname, colnames(cellexalObj@userGroups)))){
            cellexalObj@userGroups[,sel@gname] = NA
            cellexalObj@userGroups[,paste(sel@gname, 'order')] = NA
            cellexalObj@userGroups[names(sel),sel@gname] = sel@dat['col']
            cellexalObj@userGroups[names(sel),paste(sel@gname, 'order')] = sel@dat['order']
        }

        if (length( grep ("pseudoBulk", sel@gname) ) == 0 ){
            cellexalObj@usedObj$lastGroup = sel@gname
        }
        #print(paste("added a new time object", sel@gname))
        return(invisible(cellexalObj))
	}
)


#' @rdname addSelection
setMethod('addSelection', signature = c ('cellexalvrR', 'cellexalGrouping'), 
	definition = function ( cellexalObj, sel ) {
        overlapp = lapply( cellexalObj@usedObj$linearSelections, function(x) { describesTheseCells(x, names(sel) ) } )
        same = names(overlapp)[unlist(overlapp)]

        if ( length(same) >1 ){
            len = length(same)
            for ( name in same) {
                if (name == "lastEntry"){
                    len = len -1
                }
            }

            if (len >1){
                print("Multiple cellexalGrouping objects describing the same cells - does that make sense here?")
            }

        }
        if ( length(same) == 1 ){
            sel@gname = same
        }
        cellexalObj@groupSelectedFrom[[sel@gname]] = sel
        cellexalObj@usedObj$lastGroup = sel@gname

        if ( is.na( match( sel@gname, colnames(cellexalObj@userGroups)))){
            cellexalObj@userGroups[,sel@gname] = sel@grouping
            cellexalObj@userGroups[,paste(sel@gname, 'order')] = sel@order
        }

        if ( !empty(sel@linarObj) ) {
            addSelection( cellexalObj, sel@linarObj )
        }
        return(invisible(cellexalObj))

	}
)
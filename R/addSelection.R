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
                print("Library Error - I have a duplicate linear object in my internal list - fix me!")
                if ( interactive() ){
                    browser()
                }
            }
        }
        if ( length(same) == 1 ){
            print(paste( "This time object already exixts - renamed old time object to",sel@gname ))
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

        cellexalObj@usedObj$lastGroup = sel@gname
        print(paste("added a new time object", sel@gname))
        return(invisible(cellexalObj))
	}
)


#' @rdname addSelection
setMethod('addSelection', signature = c ('cellexalvrR', 'cellexalGrouping'), 
	definition = function ( cellexalObj, sel ) {
        overlapp = lapply( cellexalObj@usedObj$linearSelections, function(x) { describesTheseCells(x, names(sel) ) } )
        if ( length(same) >1 ){
            len = length(same)
            for ( name in same) {
                if (name == "lastEntry"){
                    len = len -1
                }
            }

            if (len >1){
                print("Library Error - I have a duplicate groupiong in my internal list - fix me!")
                if ( interactive() ){
                    browser()
                }
            }

        }
        if ( length(same) == 1 ){
            sel@gname = sname
        }
        cellexalObj@groupSelectedFrom[[sel@gname]] = sel
        cellexalObj@usedObj$lastGroup = sel@gname    
        return(invisible(cellexalObj))

	}
)
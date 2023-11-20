#' Subset a time selection if it exists and if the cells given are actually a subset of this time.
#' This will also create a smaller version of the normal selection to be able to plot this subset correctly.
#' Beware - it does not return the modified time - blame the stupid R language for that.
#' @name subsetTime
#' @docType methods
#' @description Identify to n diff genes per cluster
#' @param cellexalObj cellexalvrR object
#' @param timeName the name of the time you want to change
#' @param cells the cell names you want to subset the time for
#' @param newName the new name for the subset
#' @keywords DEGs
#' @title convert stats table into ToppCluster input file
#' @examples 
#' @return returns a cellexalvrR object with the modified time selection
#' @export 
setGeneric('subsetTime', function(cellexalObj, oldName, cells, newName) { 
    standardGeneric('subsetTime') 
})

#' @param cellexalObj the object
#' @param oldName the name of the time to subset
#' @param cells the cell names to subset the object to
#' @param newName the new name for the subset
#' @title subset a cellexalLinear object
#' @export 
setMethod('subsetTime', signature = c('cellexalvrR', 'character', 'character', 'character'),
    definition = function(cellexalObj, oldName, cells, newName) {

        oldTime = groupingInfo( cellexalObj, oldName )@linarObj
        print( paste( "oldName", oldName,"newName",newName))
        newTime = subset(oldTime, cells)
        newTime@gname = newName

        overlapp = lapply( cellexalObj@usedObj$linearSelections, function(x) { x@id == newTime@id } )
        same = names(overlapp)[unlist(overlapp)]
        if ( length(same) >1 ){
            stop("Library Error - I have a duplicate linear object in my internal list - fix me!")
        }
        if ( length(same) == 1 ){
            print("This time object already exixts - renamed old time object!")
            cellexalObj@usedObj$linearSelections[[n]] = newTime
            cellexalObj@groupSelectedFrom[[newTime@gname]]@linarObj = newTime
            cellexalObj@usedObj$linearSelections[['lastEntry']] = newTime    
            return(invisible(cellexalObj))
        }

        parentSelection = groupingInfo(cellexalObj, oldTime@parentSelection)

        if (!describesTheseCells(parentSelection, rownames(newTime@dat))) {
            # would be extremely surprising if that would not be the case!
            # register the new time in the new partent
            message("we need to create a new parent?")
            newParent = subset(parentSelection, rownames(newTime@dat))
            newTime@parentSelection = newParent@gname
            newParent@linarObj = newTime
            # register the new selection
            cellexalObj@groupSelectedFrom[[newParent@gname]] = newParent
            
            # Rest of the operations to fix userGroups entry for the parent
            cellexalObj@userGroups[, newParent@gname] = NA
            matching = match(rownames(newTime@dat), colnames(cellexalObj@data))
            cellexalObj@userGroups[matching, newParent@gname] = cellexalObj@userGroups[matching, newTime@parentSelection]
            cellexalObj@userGroups[, paste(newParent@gname, 'order')] = NA
            cellexalObj@userGroups[matching, paste(newParent@gname, 'order')] = cellexalObj@userGroups[matching, paste(newTime@parentSelection, 'order')]

            
            parentSelection = newParent
        }
        # now finally register the enw time in the object!

        cellexalObj@usedObj$SelectionFiles[[parentSelection@gname]] = parentSelection@selectionFile
        cellexalObj@usedObj$SelectionFiles[[newTime@gname]] = parentSelection@selectionFile
        cellexalObj@usedObj$linearSelections[['lastEntry']] = newTime
        cellexalObj@usedObj$linearSelections[[newName]] = newTime

        cellexalObj@usedObj$lastGroup = newName
        print(paste("The new name for the time is", newName))
        return(invisible(cellexalObj))
        
    }
)

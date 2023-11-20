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
setGeneric('subsetTime', function(tab, count = 500, nameAdd = NULL) { 
    standardGeneric('subsetTime') 
})

#' @param cellexalObj the object
#' @param timeName the name of the time to subset
#' @param cells the cell names to subset the object to
#' @param newName the new name for the subset
#' @title subset a cellexalLinear object
#' @export 
setMethod('subsetTime', signature = c('cellexalvrR', 'character', 'character', 'character'),
    definition = function(cellexalObj, timeName, cells, newName) {
        oldTime = groupingInfo(cellexalObj, timeName)
        newTime = subset(oldTime, cells)
        newTime@gname = newName

        done = FALSE
        for (n in names(cellexalObj@usedObj$linearSelections)) {
            if (cellexalObj@usedObj$linearSelections[[n]]@id == newTime@id) {
                newTime@gname = cellexalObj@usedObj$linearSelections[[n]]@gname
                cellexalObj@usedObj$linearSelections[[n]] = newTime
                cellexalObj@groupSelectedFrom[[newTime@gname]]@linarObj = newTime
                cellexalObj@usedObj$linearSelections[['lastEntry']] = newTime
                done = TRUE
            }
        }
        if (done) {
            return(invisible(cellexalObj))
        } else {
            parentSelection = groupingInfo(cellexalObj, oldTime@parentSelection)

            if (!describesTheseCells(parentSelection, rownames(newTime@dat))) {
                print("Assuming the original timeline has been subsetted?")

                newParent = subset(parentSelection, rownames(newTime@dat))
                newTime@parentSelection = newParent@gname
                newParent@linarObj = newTime
                
                # Rest of the operations to fix userGroups entry for the parent
                cellexalObj@userGroups[, newParent@gname] = NA
                matching = match(rownames(newTime@dat), colnames(cellexalObj@data))
                cellexalObj@userGroups[matching, newParent@gname] = cellexalObj@userGroups[matching, newTime@parentSelection]
                cellexalObj@userGroups[, paste(newParent@gname, 'order')] = NA
                cellexalObj@userGroups[matching, paste(newParent@gname, 'order')] = cellexalObj@userGroups[matching, paste(newTime@parentSelection, 'order')]

                cellexalObj@usedObj$SelectionFiles[[newParent@gname]] = newParent
                cellexalObj@usedObj$SelectionFiles[[newTime@gname]] = newParent
                parentSelection = newParent
            }
            print(paste("The new name for the time is", newName))
            return(invisible(cellexalObj))
        }
    }
)

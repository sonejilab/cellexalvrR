## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE------------------------------------------------------------
library(cellexalvrR)

cellexalObj = reset( cellexalObj )
cellexalObj


## ----message=FALSE------------------------------------------------------------
head( read.delim( system.file("extdata", "SelectionHSPC_time.txt", package="cellexalvrR"), header=F ) )


## ----message=FALSE------------------------------------------------------------
cellexalObj@outpath = getwd()
cellexalObj = sessionPath( cellexalObj, 'linearExample')

## ----message=FALSE------------------------------------------------------------
selectionFile = system.file("extdata", "SelectionHSPC_time.txt",package= "cellexalvrR")
cellexalObj = userGrouping( cellexalObj, selectionFile )
cellexalObj = pseudotimeTest3D(cellexalObj, grouping= cellexalObj@usedObj$lastGroup )
cellexalObj = createStats( "lastEntry" , cellexalObj,  num.sig= 250 )

bossLinearSelection = cellexalObj@usedObj$linearSelections[["lastEntry"]]
cellexalObj = createReport(bossLinearSelection, cellexalObj, info = bossLinearSelection)$cellexalObj


# the statistics table
print ( head( cellexalObj@usedObj$sigGeneLists$lin[[ cellexalObj@usedObj$lastGroup ]] ))

length( cellexalObj@usedObj$deg.genes)

## ---- message=FALSE-----------------------------------------------------------
cellexalObj@usedObj$linearSelections

## ---- message=FALSE-----------------------------------------------------------
linearSelection = getLinearSelection(cellexalObj, bossLinearSelection)

subset1 = rownames(linearSelection@dat)[seq(1,nrow(linearSelection@dat),2)]
subset2 = rownames(linearSelection@dat)[seq(2,nrow(linearSelection@dat),2)]

linearSubset1 = subset( linearSelection, subset1)
linearSubset2 = subset( linearSelection, subset2)

cellexalObj = addSelection( linearSubset1, cellexalObj )
linearSubset1 = cellexalObj@usedObj$linearSelections[[1]]

cellexalObj = addSelection( linearSubset2, cellexalObj )
linearSubset2 = cellexalObj@usedObj$linearSelections[[1]]

cellexalObj = createStats( linearSubset1, cellexalObj,  num.sig= 250 )
deg.genes = cellexalObj@usedObj$deg.genes

cellexalObj = createReport( linearSubset1, cellexalObj, 
     linearSubset1, deg.genes = deg.genes )$cellexalObj

cellexalObj = createStats( linearSubset2, cellexalObj,  num.sig= 250 )
cellexalObj = createReport(linearSubset2, cellexalObj, 
     linearSubset2, deg.genes = deg.genes )$cellexalObj

cellexalObj = compareGeneClusters ( 
   getLinearSelection(cellexalObj,linearSubset1@gname), 
   getLinearSelection(cellexalObj,linearSubset2@gname), 
	 cellexalObj, altGroupNames=c("subset A", "subset B" ) 
)


## ---- message=FALSE-----------------------------------------------------------
cellexalObj = renderReport( cellexalObj)


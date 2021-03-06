args <- commandArgs(trailingOnly = TRUE)

selectionfile <- args[1]
userfolder <- args[2]
datafolder <- args[3]

suppressMessages(library( cellexalvrR ))

message( paste("update grouping with grouping file",selectionfile ))

#print("started")
if ( file.exists( file.path( userfolder, 'cellexalObj.RData' )) ){
	load(  file.path( userfolder, 'cellexalObj.RData' ))
#	print("load from user folder")
}else {
	load(  file.path( datafolder, 'cellexalObj.RData' ))
#	print("load from data folder")
}

cellexalObj <- userGrouping( cellexalObj, selectionfile )

t <- exportUserGroups4vr ( cellexalObj, userfolder )

if ( isS4(cellexalObj) ) {
	file.copy(selectionfile, file.path( userfolder,paste(sep='.', cellexalObj@usedObj$lastGroup,'txt' ) ) )
}else {
	file.copy(selectionfile, file.path( userfolder,paste(sep='.', cellexalObj$usedObj$lastGroup,'txt' ) ) )
}
#message( "Save updated cellexalvrR object" )
#lockedSave ( cellexalObj, path= userfolder)

#print("done")

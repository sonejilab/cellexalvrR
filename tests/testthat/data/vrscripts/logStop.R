suppressMessages(library(cellexalvrR))
message( "Stop the logging")

args <- commandArgs(trailingOnly = TRUE)

datadir <- args[1] ## please give me the user spcific analysis path here!!!!

cellexalObj <- loadObject(file.path(datadir, "cellexalObj.RData"))

cellexalObj = renderReport( cellexalObj ) ## finalize the session also removing the session information from the cellexalObj

lockedSave(cellexalObj)

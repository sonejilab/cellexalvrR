#' @name VRmode
#' @docType methods
#' @description Windows specific check if CellexalVR.exe is running
#' @returns boolean value if the VR application is running. 
#' @keywords VRmode
#' @title check if CellexalVR.exe is running (Windows specific)
#' @export 
VRmode = function( ){
	tryCatch ( {
		task <- shell('tasklist /fi "imagename eq CellexalVR.exe" /nh /fo csv',
			intern = TRUE)
		OK = grepl("CellexalVR.exe", task)
		}, 
	warning = function(err){
		## likely not working! likely tasklist not available and hence not windows
		OK = FALSE
    	}
	)
	OK
}

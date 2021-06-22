
#' create the ''' wrapped location of the Rscript/Rscript.exe program
#' @export
Rscript.exe = function() {
    rscript = file.path(R.home(), "bin", "Rscript")
    if (!file.exists(rscript)) {
        rscript = paste(rscript, sep = ".", "exe")
    }
    paste("", rscript, "", sep = "\"")
}

#' create the ''' wrapped location of the R/R.exe program
#' @export
R.exe = function() {
    rscript = file.path(R.home(), "bin", "R")
    if (!file.exists(rscript)) {
        rscript = paste(rscript, sep = ".", "exe")
    }
    paste("", rscript, "", sep = "\"")
}

#' Convert a file 'A/B/C.txt' into the string "file.path('A','B','C.txt')".
#' @param path the file to convert
#' @param mustWork default = FALSE
#' @export
file2Script = function(path, mustWork = FALSE) {
    output <- c(strsplit(dirname(normalizePath(path, mustWork = mustWork)), .Platform$file.sep)[[1]], 
        basename(path))
    paste("file.path('", paste(output, collapse = "','"), "')", sep = "")
}

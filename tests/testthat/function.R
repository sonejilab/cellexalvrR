#' checkFile checks a test file and fetches the amount of matches per string.

checkFile = function ( collect, ofile, statementSplit = '>' ) {

  if ( ! file.exists( ofile) ) {
    stop(paste( "the outfile", ofile,"does not exist and can therefore not be examined."))
  }
	con = file(ofile, "r")
	while ( TRUE ) {
  	line = readLines(con, n = 1)
 	if ( length(line) == 0 ) {
   		break
  	}
  	for ( na in names(collect) ){
  		if ( length(grep( na, line))> 0){
        if ( length( grep( statementSplit, na )) > 0 ){
          collect[[na]] = collect[[na]] + 1
        }
        else {
          collect[[na]] = collect[[na]] + 
           length(grep( na, unlist(stringr::str_split(line, ">"))))
        }   
  		}
  	}
	}
	close(con)

	collect
}

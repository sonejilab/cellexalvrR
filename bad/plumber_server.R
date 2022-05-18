#' This has to be implemented before usage :-D
#' The start of the server might also be something another program has to initiate.


## load the cellexalObj you want to serve.
## start a session, too - otherwise this tool is rather worthless...
obj = plumber::pr( system.file("extdata", "webapi.R",package= "cellexalvrR") ); plumber::pr_run(obj, port=8000)
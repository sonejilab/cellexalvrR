library(devtools)
document()
unlink( file.path( 'test', 'testthat', 'data', 'output'), recursive = TRUE )
build()
context( 'Test ShinCell export' )

prefix = './'
# do not set that as the shiny cell will not work otherwise
Sys.setenv(TEST_MODE = "TRUE")

cellexalObj@outpath = file.path(prefix,"data","output")

if ( file.exists( file.path(prefix,"data","output","shinyExport") )){
	unlink(  file.path(prefix,"data","output","shinyExport" ), recursive=TRUE )
}

cellexalObj = sessionPath( cellexalObj, "shinyExport" )

expect_true( file.exists( file.path( prefix,"data","output","shinyExport" )), label="Session Path was created")

grouping <- file.path(prefix, 'data','selection00.txt')

cellexalObj = userGrouping( cellexalObj, grouping)

as_ShinyCell( cellexalObj,  file.path( prefix,"data","output","shinyExport" ) )

expect_true( ! file.exists( file.path( prefix,"data","output","shinyExport", "shinyApp" )), label="Shiny path should  not have been generated (TEST_MODE == TRUE)!")

Sys.setenv(TEST_MODE = "FALSE")

as_ShinyCell( cellexalObj ,  file.path( prefix,"data","output","shinyExport" ) )

expect_true(  file.exists( file.path( prefix,"data","output","shinyExport", "shinyApp" )), label="Shiny path was generated (TEST_MODE == FALSE)!")


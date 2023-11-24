context('subset')

x = cellexalObj

prefix = './'

x = reset(x)
x@outpath = file.path(prefix,'data','output','subset' )
x = sessionPath( x, 'test')

x = userGrouping( x, file.path(prefix, 'data', 'SelectionHSPC_time.txt' ))

selection = groupingInfo( x )

expect_true( class(selection)[1] == "cellexalGrouping", "right selection class")

sub1 = names( selection )[ seq(1, length( names(selection)), by = 2) ]

subset1 = subset ( selection, sub1 )

expect_true( length(which(!is.na(subset1@grouping))) == length(sub1), "selected the right amount" )

expect_true( all ( table(c( names(subset1@grouping)[which(!is.na(subset1@grouping))],sub1) ) == 2 ), "selected the right cells" )

expect_true( subset1@gname == "User.group.2", info="gname has been changed")

## the selection should also have gotten a selection file in the session path!
expect_true( ! file.exists( file.path(x@usedObj$sessionPath, "session2.txt") ), info="The subset has not been exported" )

expect_equal (subset1@selectionFile, "selection2.txt", label="selection file has ben changed" )

x = addSelection( x, subset1 )

expect_true( file.exists( file.path(x@usedObj$sessionPath, "session2.txt") ), info="The subset has been exported" )


skip("first implement the loadTime function - for later")
x = loadTime( x, file.path(prefix, 'data', 'SelectionHSPC_time.txt.time' ))


context('groupingInfo')


prefix = '.'

x = reset(cellexalObj)
x@outpath = file.path(prefix,'data','output','groupingInfo' )

if ( ! file.exists(x@outpath)){
	dir.create(x@outpath)
}

x = sessionPath( x, 'cellexalGroupTest' )

grouping <- file.path(prefix, 'data/selection1.txt')
x = userGrouping(x, grouping )

grouping <- file.path(prefix, 'data', 'SelectionHSPC_time.txt' )

x = userGrouping(x, grouping )


g1 = groupingInfo( x, 'User.group.1')
if ( class(g1)[1] == 'cellexalvrR' ){
	x = g1
	g1 = groupingInfo( x, 'User.group.1')
}
expect_true( g1@gname == 'User.group.1', label = "first name correct")

g2 = groupingInfo( x, 'User.group.2')
if ( class(g2)[1] == 'cellexalvrR' ){
	x = g2
	g2 = groupingInfo( x, 'User.group.1')
}

expect_true( g2@gname == 'User.group.2', label = "first name correct")

g3 = groupingInfo( x, c( 'User.group.1', 'User.group.2' ) )

expect_true( length(g3) == 2, label="get two selections")


expect_equal( unlist( lapply( g3, function(x){ x@gname})) , c( 'User.group.1', 'User.group.2' ),label="two names")

expect_equal( g3 , c( g1, g2 ),label="two objects")


g4 = groupingInfo( x, g3 )

context('groupingInfo - grouing defined in R')

x@userGroups$Char = sample( LETTERS[1:3], size=ncol(x@data), replace=T)
x@userGroups$"Char order" = 1:ncol(x@data)

g5 = groupingInfo( x, 'Char' )

expect_equal( class(g5)[1], "cellexalvrR", label="first time run returns cellexalvrR object")

if ( class(g5)[1] == "cellexalvrR" ){
	x = g5
	g5 = groupingInfo( x, 'Char' )
}

expect_equal( class(g5)[1], "cellexalGrouping", label="first time run returns cellexalvrR object")

expect_equal( class( g5@grouping ), 'factor' )
expect_equal( names(table ( g5@grouping ) ), c('A', 'B', 'C') )


expect_equal( unlist( lapply( g3, function(x){ x@gname})) , c( 'User.group.1', 'User.group.2' ),label="two names")

context('GrabDRCs')

set.seed(100)
#prefix = 'tests/testthat'
prefix = '.'


to = sample(colnames(cellexalObj@data), 100)

part = reduceTo( cellexalObj, what='col', to=to)

merger = GrabDRCs( cellexalObj, part, prefix='part')

for ( i in 1:3){
	 expect_equal( dim(merger@drc[[i]]), c(1654, 3))
}

for ( i in 4:6){
	 expect_equal( dim(merger@drc[[i]]), c(100, 3))
}

to2 = colnames(cellexalObj@data)[ c(1:49,150:200) ]

part2 = reduceTo( merger, what='col', to=to2)

OK = colnames(part2@data)

for ( i in 1:3){
	expect_equal(rownames( part2@drc[[i]]), OK)
}

OK = intersect(to2, to)

for ( i in 4:6){
	 expect_equal( rownames(part2@drc[[i]]), OK)
}


for ( i in 1:3){
	 expect_equal( dim(part2@drc[[i]]), c(100, 3))
}

for ( i in 4:6){
	 expect_equal( dim(part2@drc[[i]]), c(5, 3))
}

to3 = setdiff(to2, to) 

expect_true( length(to3) == 95)

part3 = reduceTo( merger, what='col', to=to3)

for ( i in 1:3){
	 expect_equal( dim(part3@drc[[i]]), c(95, 3))
}

for ( i in 4:6){
	 expect_equal( dim(part3@drc[[i]]), c(0, 3))
}

merger@outpath = tempdir(check = FALSE)


context('GrabDRCs - Object usability - timelines')

merger = sessionPath(merger)

merger@userGroups=data.frame()
merger@usedObj$groupSelectedFrom = list()
merger@usedObj$timelines = list()

#selectionF = file.path(prefix,'data','SelectionHSPC_time.txt')
#merger = getDifferentials(merger, selectionF, 'wilcox')


#check(merger)

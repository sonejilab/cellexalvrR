context('check function')

m = matrix( floor(runif(30000, min=-1000, max=101)), ncol=300)
colnames(m) = paste('cell', 1:ncol(m))
rownames(m) = paste('gene', 1:nrow(m))
m[which(m< 1)] = 0
m = Matrix::Matrix(m,sparse=T)
obj = new( 'cellexalvrR', data=m , drc= list('test' = cbind(x=runif(300), y=runif(300), z=runif(300) )) )

meta.cell= data.frame( A= rep(c('a','b'), 150), 'B' = rep(c('a','b','c'), 100) )
obj@meta.cell = as.matrix(meta.cell)

dump = tempfile(patter='dump')
file.create(dump)
dumpf=file( dump)
open(dumpf)


sink(dumpf, type='message')
obj=check(obj)
sink(type='message')

expect_equal( obj@usedObj$checkPassed, FALSE, 'check failed as expected')

expect_equal( obj@usedObj$checkError, c(
	"the data colnames are not the same as the meta.cell rownames!",
	"meta.cells is not a 0/1 table",
	"drc test has no rownames - please fix that"
	))

rownames(obj@meta.cell ) = colnames(obj@data)

sink(dumpf, type='message')
obj=check(obj)
sink(type='message')

expect_equal( obj@usedObj$checkPassed, FALSE, 'check failed as expected')

expect_equal( obj@usedObj$checkError, c(
	"meta.cells is not a 0/1 table",
	"drc test has no rownames - please fix that"
	))

obj@meta.cell =make.cell.meta.from.df( as.data.frame( obj@meta.cell), c('A','B'))

sink(dumpf, type='message')
obj=check(obj)
sink(type='message')

expect_equal( obj@usedObj$checkPassed, FALSE, 'check failed as expected')

expect_equal( obj@usedObj$checkError, c(
	"drc test has no rownames - please fix that"
	))

rownames(obj@drc[[1]]) = colnames(obj@data)

sink(dumpf, type='message')
obj=check(obj)
sink(type='message')

expect_equal( obj@usedObj$checkPassed, TRUE, 'check failed as expected')

expect_equal( obj@usedObj$checkError, NULL)

obj@drc[[1]][1,1] = NA

sink(dumpf, type='message')
obj=check(obj)
sink(type='message')

expect_equal( obj@usedObj$checkPassed, FALSE, 'check failed as expected')

expect_equal( obj@usedObj$checkError, c(
	"NA values in drc test  - please fix that"
	))

close(dumpf)
context('merge two cellexalvrR objects')


new = merge ( cellexalObj , cellexalObj)

all.equal( table( new@usedObj$samples$sname), table( c(rep("other", 1654), rep("this", 1654))) )
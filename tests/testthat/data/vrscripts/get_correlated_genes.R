suppressMessages(library(cellexalvrR))

library(cellexalvrR)

args <- commandArgs(trailingOnly = TRUE)

datadir <- args[1]

gene_name <- args[2]

outputfile <- args[3]

facsTypeArg <- as.logical(args[4])

expression_data_filepath <- file.path(datadir, "cellexalObj.RData")

#print (paste( "I got the facsTypeArg", facsTypeArg ) )

get.genes.cor.to(cellexalObj = expression_data_filepath, 
		gname = gene_name, output = outputfile, is.smarker=facsTypeArg, cpp=T)
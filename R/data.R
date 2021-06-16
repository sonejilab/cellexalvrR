#' @importFrom AnnotationDbi mapIds
##' @importFrom DDRTree DDRTree
#' @importFrom FastWilcoxTest CorMatrix StatTest collapse meltSparseMatrix
#' @importFrom Matrix Matrix drop0 rowSums t
#' @importFrom RSQLite dbConnect dbDisconnect dbSendStatement dbWriteTable
##' @importFrom alphashape3d ashape3d
#' @importFrom bookdown render_book
##' @importFrom entropy entropy
#' @importFrom grDevices png rainbow
#' @importFrom graphics hist
#' @importFrom knitr kable
#' @importFrom methods new slotNames validObject
#' @importFrom stats cor hclust
#' @importFrom stringr str_replace_all str_split
#' @importFrom topGO GenTable runTest
#' @importFrom methods .hasSlot is slot<- 
#' @importFrom stats quantile var
#' @importFrom utils packageVersion 

NULL


#' A list of mouse transcription factors.
#'
#' A list of mouse transcription factors. Used when generating TF networks
#'
#' @format A vector 1357 in length:
#' \describe{
#'   \item{tf}{transcription factor}
#'   ...
#' }
#' @source \url{http://bioinfo.life.hust.edu.cn/AnimalTFDB/}
#' @keywords data
"mouse.tfs"

globalVariables("mouse.tfs", 'cellexalvrR' , add = TRUE)

NULL

#' A list of human transcription factors.
#'
#' A list of human transcription factors. Used when generating TF networks
#'
#' @format A vector 1468 in length:
#' \describe{
#'   \item{tf}{transcription factor}
#'   ...
#' }
#' @source \url{http://bioinfo.life.hust.edu.cn/AnimalTFDB/}
#' @keywords data
"human.tfs"

globalVariables("human.tfs", 'cellexalvrR' , add = TRUE)


#' @name Epigenetic
#' @title A simple table containing the data from 'http://epifactors.autosome.ru/' as from 21st September 2017
#' @description This table can be used to create the epigenetics DRC objects.
#' @docType data
#' @usage Epigenetic
#' @format data.frame
#' @keywords data
'Epigenetic'

globalVariables("Epigenetic", 'cellexalvrR' , add = TRUE)

#' @name CellCycle
#' @title A simple table containing the Human and mouse orthologe CellCycle genes from PMID17994010
#' "Genome-scale RNAi profiling of cell division in human tissue culture cells."
#' @description The data can be used by stating
#' useInbuiltGOIlists (cellexalObj, 'CellCycle' ) #function definition in file 'useInbuiltGOIlists.R'
#' And it is used to visualize the cell cycle genes in the VR environment.
#' Only the genes also identifyable in mouse were used here.
#' @docType data
#' @usage CellCycle
#' @format data.frame
#' @keywords data
'CellCycle'

globalVariables("CellCycle", 'cellexalvrR' , add = TRUE)

#' @name cellexalObj
#' @title The cellexlvrR object described in the vignette
#' @description THE test dataset
#' @docType data
#' @usage cellexalObj
#' @format cellexalvrR
#' @keywords data
'cellexalObj' 

globalVariables("cellexalObj", 'cellexalvrR' , add = TRUE)



#' @name human.CellSurface
#' @title A simple list containing the human ENTREZ.gene.symbols from 'A Mass Spectrometric-Derived Cell Surface Protein Atlas' plos 2015
#' @description This list is used to crete a CellSurface object
#' @docType data
#' @usage human.CellSurface
#' @format vector
#' @keywords data
"human.CellSurface"

globalVariables("human.CellSurface", 'cellexalvrR' , add = TRUE)


#' @name mouse.CellSurface
#' @title A simple list containing the mouse ENTREZ.gene.symbols from 'A Mass Spectrometric-Derived Cell Surface Protein Atlas' plos 2015
#' @description This list is used to crete a CellSurface object
#' @docType data
#' @usage mouse.CellSurface
#' @format vector
#' @keywords data
"mouse.CellSurface"

globalVariables("mouse.CellSurface", 'cellexalvrR' , add = TRUE)

